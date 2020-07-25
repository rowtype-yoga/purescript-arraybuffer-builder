-- | `ArrayBuffer` serialization primitives in the “builder monad” style.
-- | An API on top of
-- | [`Data.ArrayBuffer.ArrayBuffer`](https://pursuit.purescript.org/packages/purescript-arraybuffer/docs/Data.ArrayBuffer.ArrayBuffer)
-- | which provides a `Builder` monoid and a `PutM` monad which are roughly
-- | equivalent to the Haskell
-- | [`Data.Binary.Put`](https://hackage.haskell.org/package/binary/docs/Data-Binary-Put.html)
-- | module.
-- | This library defines no typeclasses.
-- |
-- | See purescript-parsing-dataview for deserialization.
-- |
-- | # Usage Examples
-- |
-- | All `ArrayBuffer` building must occur in `Effect`.
-- |
-- | ## Serialize an integer
-- |
-- | Create a two-byte `buf :: ArrayBuffer` which contains the number *-2* encoded as big-endian 16-bit two’s-complement.
-- | ```purescript
-- | buf <- execPut $ putInt32be (-2)
-- | ```
-- |
-- | ## Serialize a `String` as UTF8
-- |
-- | Create a `buf :: ArrayBuffer` which contains a `String` encoded as UTF8 with a length prefix in a
-- | way that's compatible with the
-- | [`Binary.Put.putStringUtf8`](https://hackage.haskell.org/package/binary/docs/Data-Binary-Put.html#v:putStringUtf8)
-- | function from the Haskell
-- | [__binary__](https://hackage.haskell.org/package/binary)
-- | library.
-- | Uses [`Data.TextEncoding.encodeUtf8`](https://pursuit.purescript.org/packages/purescript-text-encoding/docs/Data.TextEncoding#v:encodeUtf8).
-- | ```purescript
-- | buf <- execPut $ do
-- |   let stringbuf = Data.ArrayBuffer.Typed.buffer $ Data.TextEncoding.encodeUtf8 "Black Lives Matter"
-- |   -- Put a 64-bit big-endian length prefix for the length of the utf8 string, in bytes.
-- |   putUint32be 0
-- |   putUint32be $ Data.Uint.fromInt $ Data.ArrayBuffer.byteLength stringbuf
-- |   putArrayBuffer stringbuf
-- | ```
-- |
-- | ## Serialize an `Array Int`
-- |
-- | Create a `buf :: ArrayBuffer` which encodes an `Array Int` with a length prefix in a
-- | way that's compatible with the
-- | [`Binary` instance for `[Int32]`](https://hackage.haskell.org/package/binary/docs/Data-Binary.html#t:Binary)
-- | from the Haskell
-- | [__binary__](https://hackage.haskell.org/package/binary)
-- | library.
-- | ```purescript
-- | let x = [1,2,3] :: Array Int
-- | buf <- execPut $
-- |   -- Put a 64-bit big-endian length prefix for the length of the array, in bytes.
-- |   putUint32be 0
-- |   putUint32be $ Data.Uint.fromInt $ Data.Array.length x
-- |   traverse_ putInt32be x
-- | ```
module Build
( Builder
, PutM
, Put
, execBuilder
, execPutM
, execPut
, putArrayBuffer
, putUint8
, putInt8
, putUint16be
, putUint16le
, putInt16be
, putInt16le
, putUint32be
, putUint32le
, putInt32be
, putInt32le
, putFloat32be
, putFloat32le
, putFloat64be
, putFloat64le
)
where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Data.UInt (UInt)
import Data.Float32 (Float32)
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
-- | left-only binary tree, a.k.a. a linked list.
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
-- | We hope that this implementation is fairly fast, but we
-- | haven't chosen this implementation because it's fast, we've chosen
-- | this implementation because it's simple. This library provides basic
-- | ArrayBuffer serialization with the familar “put monad” style.
-- | The only currently existing Purescript libary which does ArrayBuffer
-- | serialization is
-- | https://pursuit.purescript.org/packages/purescript-arraybuffer-class
-- | , and that library is heavily typeclass-based and designed
-- | for compatibility with the Haskell cereal library.
-- | If someone wants to create a fast Purescript ArrayBuffer serialization
-- | library, then they can benchmark against this one to prove that the new
-- | one is fast.
-- |
-- | One relatively cheap and simple performance improvement for this library would be to
-- | remove the Null constructor of Builder and instead use Javascript nulls.
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

-- | Monomorphic foldM copied from
-- | https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Foldable#v:foldM
foldM :: forall b m. (Monad m) => (b -> ArrayBuffer -> m b) -> b -> Builder -> m b
foldM f a0 = foldl (\ma b -> ma >>= flip f b) (pure a0)

singleton :: ArrayBuffer -> Builder
singleton buf = Node Null buf Null

-- | Add an ArrayBuffer to the end of the Builder.
snoc :: Builder -> ArrayBuffer -> Builder
snoc bs x = Node bs x Null

-- | Build a single `ArrayBuffer` from a `Builder`.
execBuilder :: forall m. (MonadEffect m) => Builder -> m ArrayBuffer
execBuilder bldr = do
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

-- | Monad computation to build an `ArrayBuffer` in any `MonadEffect`.
execPutM :: forall m. (MonadEffect m) => PutM m Unit -> m ArrayBuffer
execPutM = execBuilder <=< execWriterT

-- | Monad computation to build an `ArrayBuffer` in the `Effect` monad.
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

-- | Append an 8-bit two’s-complement signed integer (char) to the builder.
putInt8 :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt8 x = do
  buf <- liftEffect $ AB.empty 1
  _ <- liftEffect $ DV.setInt8 (DV.whole buf) 0 x
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

-- | Append a 16-bit big-endian two’s-complement signed integer to the builder.
putInt16be :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt16be x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setInt16be (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 16-bit little-endian two’s-complement signed integer to the builder.
putInt16le :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt16le x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setInt16le (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 32-bit big-endian unsigned integer to the builder.
putUint32be :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint32be x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setUint32be (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 32-bit little-endian unsigned integer to the builder.
putUint32le :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint32le x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setUint32le (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 32-bit big-endian two’s-complement signed integer to the builder.
putInt32be :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt32be x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setInt32be (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 32-bit little-endian two’s-complement signed integer to the builder.
putInt32le :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt32le x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setInt32le (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 32-bit big-endian IEEE single-precision float to the builder.
putFloat32be :: forall m. (MonadEffect m) => Float32 -> PutM m Unit
putFloat32be x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setFloat32be (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 32-bit little-endian IEEE single-precision float to the builder.
putFloat32le :: forall m. (MonadEffect m) => Float32 -> PutM m Unit
putFloat32le x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setFloat32le (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 64-bit big-endian IEEE double-precision float to the builder.
putFloat64be :: forall m. (MonadEffect m) => Number -> PutM m Unit
putFloat64be x = do
  buf <- liftEffect $ AB.empty 8
  _ <- liftEffect $ DV.setFloat64be (DV.whole buf) 0 x
  tell $ singleton buf

-- | Append a 64-bit little-endian IEEE double-precision float to the builder.
putFloat64le :: forall m. (MonadEffect m) => Number -> PutM m Unit
putFloat64le x = do
  buf <- liftEffect $ AB.empty 8
  _ <- liftEffect $ DV.setFloat64le (DV.whole buf) 0 x
  tell $ singleton buf