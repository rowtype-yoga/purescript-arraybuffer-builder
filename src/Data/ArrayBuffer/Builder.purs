-- | `ArrayBuffer` serialization primitives. (Not typeclass-based.)
-- |
-- | Provides a `Builder` monoid which is roughly equivalent to the
-- |
-- | Provides a `PutM` monad which is roughly equivalent to `PutM` from the
-- | Haskell __binary__ library.
-- |
-- | See purescript-parsing-dataview for deserialization.
-- |
-- | # Usage Examples
-- |
-- | All `ArrayBuffer` building must occur in `Effect`.
-- |
-- | ## Serialize an integer
-- |
-- | Create a two-byte `ArrayBuffer` which contains the number *-2* encoded as big-endian 16-bit two's-complement.
-- | ```purescript
-- | buf <- execPut $ putInt32be (-2)
-- | ```
-- |
-- | ## Serialize a UTF8 string
-- |
-- | Create an `ArrayBuffer` which contains a `String` encoded as UTF8 in a way that's compatible
-- | with the
-- | [`Binary.Put.putStringUtf8`](https://hackage.haskell.org/package/binary-0.8.7.0/docs/Data-Binary-Put.html#v:putStringUtf8)
-- | function from the Haskell
-- | [__binary__](https://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString-Builder-Prim.html#v:primMapListBounded)
-- | library.
-- | Uses [`Data.TextEncoding.encodeUtf8`](https://pursuit.purescript.org/packages/purescript-text-encoding/1.0.0/docs/Data.TextEncoding#v:encodeUtf8).
-- | ```purescript
-- | buf <- execPut $ do
-- |   stringbuf <- = Data.ArrayBuffer.Typed.buffer $ encodeUtf8 "ACAB"
-- |   -- Adds a 64-bit length prefix for the length of the utf8 string, in bytes
-- |   putUint32be 0
-- |   putUint32be $ Data.ArrayBuffer.byteLength stringbuf
-- |   putArrayBuffer stringbuf
-- | ```

module Build
( Builder
, PutM
, Put
, execPutM
, execPut
)
where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Data.UInt (UInt)
import Data.Maybe (Maybe(..))
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)

import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as AT

-- | Writer monad transformer for `Builder`.
type PutM = WriterT Builder

-- | `Put` monad is the non-transformer version of `PutM`.
type Put = PutM Effect

 -- Note: use liftEffect to call the ArrayBuffer Effect functions

-- | Builder for `ArrayBuffer`s.
-- |
-- | It is common to build an `ArrayBuffer` by issuing a sequence of `put`
-- | commands in a `Put` monad, and this is the case which the Builder monoid
-- | is optimized for.
-- |
-- | # Implementation Details
-- |
-- | Implements an unbalanced binary tree to get
-- | * *O(1)* `snoc` (add element to end)
-- | * *O(1)* monoid `append` (concatenation)
-- | * *O(n)* `fold`
-- |
-- | For monoid `append`, what we actually get is *O(k)* where *k* is
-- | proportional to the number of `append`s which have already been performed.
-- |
-- | `append`ing a singleton `Builder` to the right is treated as a `snoc`.
-- | Specifically, the special case of `append l r` where `r` has a `Null`
-- | left subtree doesn't increase *k*. So as long as we're right-appending
-- | singleton `Builder`s, then we still get *O(1)* `append`.
-- |
-- | `append` is O(n), in the worst case, if the `Builder` has been built
-- | entirely by `append`ing. If the Builder has been built mostly by
-- | `snoc`ing, then `append` approaches *O(1)*.
-- |
-- |
-- | If a Builder is built entirely by `snoc`ing, it will look like a
-- | left-only tree, a.k.a. a linked list.
-- |
-- | ```
-- |            ⬤
-- |           ╱
-- |          ⬤
-- |         ╱
-- |        ⬤
-- |       ╱
-- |      ⬤
-- | ```
-- |
-- | If two of these `snoc`-built trees are `append`ed, then the new tree
-- | will look like
-- |
-- | ```
-- |            ⬤
-- |           ╱  ╲
-- |          ⬤  ⬤
-- |         ╱   ╱
-- |        ⬤  ⬤
-- |       ╱   ╱
-- |      ⬤  ⬤
-- |         ╱
-- |        ⬤
-- | ```
-- |
-- | This is all similar to
-- | https://hackage.haskell.org/package/bytestring-tree-builder
-- | , except that the `Tree` https://hackage.haskell.org/package/bytestring-tree-builder-0.2.7.3/docs/src/ByteString.TreeBuilder.Tree.html#Tree
-- | structure in __bytestring-tree-builder__ only carries values in its
-- | leaves, and therefore I think has an *O(n log(n))* fold. This `Builder`
-- | has an *O(n)* fold.
-- |
data Builder
  = Node Builder ArrayBuffer Builder
  | Null

instance semigroupBuilder :: Semigroup Builder where
  append Null Null = Null
  append l Null = l
  append Null r = r
  append l (Node Null rx rr) = Node l rx rr -- this is the snoc case
  append (Node ll lx Null) r = Node ll lx r -- found the rightmost Null
                                            -- of the left tree
  append (Node ll lx lr) r = Node ll lx $ append lr r -- search down the right
                                                      -- side of the left tree
                                                      -- to find the rightmost
                                                      -- Null

instance monoidBuilder :: Monoid Builder where
  mempty = Null

-- | `Builder` is not an instance of `Foldable` because `Builder` is monomorphic.
foldl :: forall b. (b -> ArrayBuffer -> b) -> b -> Builder -> b
foldl f a Null = a
foldl f a (Node l x r) = foldl f (f (foldl f a l) x) r

-- | foldM : "Note this function is not generally stack-safe..." ???
foldM :: forall b m. (Monad m) => (b -> ArrayBuffer -> m b) -> b -> Builder -> m b
foldM f a0 = foldl (\ma b -> ma >>= flip f b) (pure a0)

singleton :: ArrayBuffer -> Builder
singleton buf = Node Null buf Null

-- | Add an ArrayBuffer to the end of the Builder.
snoc :: Builder -> ArrayBuffer -> Builder
snoc bs x = Node bs x Null

-- | Run a computation to build an `ArrayBuffer` in any `MonadEffect`.
execPutM :: forall m. (MonadEffect m) => PutM m Unit -> m ArrayBuffer
execPutM putmonad = do
  bldr <- execWriterT putmonad
  let buflen = foldl (\b a -> b + AB.byteLength a) 0 bldr
  -- Allocate the final ArrayBuffer
  buf <- liftEffect $ AB.empty buflen
  -- Then copy each ArrayBuffer into the final ArrayBuffer.
  -- To memcpy from one ArrayBuffer to another, apparently we have to first view
  -- both ArrayBuffers as ArrayView (Typed Array).
  newview <- liftEffect (AT.whole buf :: Effect Uint8Array)
  _ <- foldM
      ( \offset a -> do
          let offset' = offset + AB.byteLength a
          aview <- liftEffect (AT.whole a :: Effect Uint8Array)
          _ <- liftEffect $ AT.setTyped newview (Just offset') aview
          pure offset'
      ) 0 bldr
  pure buf

-- | Run a computation to build an `ArrayBuffer` in the `Effect` monad.
execPut :: Put Unit -> Effect ArrayBuffer
execPut = execPutM

-- | Append an `ArrayBuffer` to the builder.
putArrayBuffer :: forall m. (MonadEffect m) => ArrayBuffer -> PutM m Unit
putArrayBuffer = tell <<< singleton

-- | Append an 8-bit unsigned integer (byte) to the builder.
putUint8 :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint8 x = do
  buf <- liftEffect $ AB.empty 1
  _ <- liftEffect $ DV.setUint8 (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 16-bit big-endian unsigned integer to the builder.
putUint16be :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint16be x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setUint16be (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 16-bit little-endian unsigned integer to the builder.
putUint16le :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint16le x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setUint16le (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 16-bit big-endian signed integer to the builder.
putInt16be :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt16be x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setInt16be (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 16-bit little-endian signed integer to the builder.
putInt16le :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt16le x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setInt16le (DV.whole buf) 0 x
  tell $ singleton buf