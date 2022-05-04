-- | Internal module.
-- |
-- | You probably don’t want to import anything from this module.
-- |
-- | ## Implementation Details
-- |
-- | We want our `Builder` to be a data structure with
-- | * *O(1)* monoid append
-- | * *O(n)* fold
-- |
-- | Our `Builder` implementation is an unbalanced binary tree.
-- |
-- | For monoid `append`, what we actually get is *O(1)* when either the
-- | left or right tree is a singleton. If that's not true, then in the
-- | unlikely worst case `append` might be *O(n)*.
-- |
-- | `Builder` is optimized for what we consider to be normal usage, that is,
-- | `snoc`ing singleton elements to the end of the `Builder`.
-- |
-- | If a Builder is built entirely by `snoc`ing, it will look like a
-- | left-only binary tree, a.k.a. a linked list.
-- |
-- | ```
-- |            ④
-- |           ╱
-- |          ③
-- |         ╱
-- |        ②
-- |       ╱
-- |      ①
-- | ```
-- |
-- | If two of these `snoc`-built trees are `append`ed, then the new tree
-- | will look like
-- |
-- | ```
-- |            ④
-- |           ╱  ╲
-- |          ③  ⑧
-- |         ╱   ╱
-- |        ②  ⑦
-- |       ╱   ╱
-- |      ①  ⑥
-- |         ╱
-- |        ⑤
-- | ```
-- |
-- | This is all similar to
-- | [__bytestring-tree-builder__](https://hackage.haskell.org/package/bytestring-tree-builder)
-- | , except that the
-- | [`Tree`](https://hackage.haskell.org/package/bytestring-tree-builder-0.2.7.3/docs/src/ByteString.TreeBuilder.Tree.html#Tree)
-- | structure in __bytestring-tree-builder__ only carries values in its
-- | leaves, which is how it achieves consistent *O(1)* appending, at the cost of
-- | a higher coefficient time factor on the fold.
-- |
-- | We hope that this implementation is fairly fast, since it is similar to
-- | __bytestring-tree-builder__
-- | which “according to the benchmarks … beats all the alternatives.”
-- | However, we
-- | haven’t chosen this implementation because it’s fast, we've chosen
-- | this implementation because it’s simple.
-- | If someone wants to create a fast PureScript `ArrayBuffer` serialization
-- | library, then they can benchmark against this one to prove that the new
-- | one is fast.
-- |
-- | One relatively cheap and simple performance improvement for this library would be to
-- | remove the `Null` constructor of `Builder` and instead use Javascript nulls?
-- |
-- | In the longer term, it might make sense to try to change the `Builder` so
-- | that it works like the
-- | https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Builder.html .
-- | That’s the approach taken by
-- | https://pursuit.purescript.org/packages/purescript-dynamic-buffers .
-- |
-- | Here are some benchmarks of different Haskell ByteString builders
-- | https://github.com/haskell-perf/strict-bytestring-builders
-- |
-- | We've tried to design the API for this library with minimal assumptions,
-- | so that if we want to change the `Builder` implementation later then we can.
-- |
module Data.ArrayBuffer.Builder.Internal
  ( Builder(..)
  , (<>>)
  , DataBuff(..)
  , toView
  , execBuilder
  , length
  , foldl
  , foldM
  , singleton
  , cons
  , snoc
  , encodeUint8
  , encodeInt8
  , encodeUint16be
  , encodeUint16le
  , encodeInt16be
  , encodeInt16le
  , encodeUint32be
  , encodeUint32le
  , encodeInt32be
  , encodeInt32le
  , encodeFloat32be
  , encodeFloat32le
  , encodeFloat64be
  , encodeFloat64le
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, DataView, Uint8Array)
import Data.Float32 (Float32)
import Data.Identity (Identity)
import Data.List (List(..), uncons, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

-- | For distinguishing between `ArrayBuffer` and `DataView`.
data DataBuff
  = Buff ArrayBuffer
  | View DataView

-- Concerning DataViews.
-- https://v8.dev/blog/dataview

-- | View the contents of `DataBuff` as a `DataView`.
toView :: DataBuff -> DataView
toView (Buff ab) = DV.whole ab
toView (View dv) = dv

-- | Monoidal builder for `ArrayBuffer`s.
-- |
-- | We can add two types of things to the `Builder`:
-- | 1. `ArrayBuffer`
-- | 2. `DataView`
-- |
-- | We might prefer
-- | to add a `DataView` to a `Builder` when we’re adding a large slice of data
-- | from some other `ArrayBuffer`, so that we don’t
-- | need an extra intermediate copy of the slice.
data Builder
  = Node Builder DataBuff Builder
  | Null

-- | ### Left-associative `<>>` append operator
-- |
-- | __TL;DR__ You probably don't want to use the `Builder` monoid directly
-- | in your code, it’s better to use the `PutM` monad with do-notation instead.
-- |
-- | The `Builder` monoid in this library is efficient when we `snoc` single
-- | items onto the end of it, or when we only `cons` single items to the
-- | beginning, but it can be less efficient when we are mixing `cons` and
-- | `snoc`.
-- | Most of the time we want to `snoc`, but the `Semigroup` append
-- | operator `<>` is right-associative,
-- | which means it chains like `cons`.
-- |
-- | To solve this, we provide an operator `<>>` for appending `Builders`.
-- | `<>>` is exactly the same as `<>`, but left-associative,
-- | which means it chains like `snoc`.
-- |
-- | This __only matters__ when we're chaining together three
-- | or more `Builder`s in a single associative expression.
-- | Instead of
-- | ```
-- | builder₁ <> builder₂ <> builder₃
-- | ```
-- | we should always prefer to
-- | write
-- | ```
-- | builder₁ <>> builder₂ <>> builder₃
-- | ```
-- |  so that we get the efficient
-- | `snoc`ing of `Builder`s.
-- |
-- | If we build our `ArrayBuffer`s with the `PutM` monad instead of appending by
-- | using the `Semigroup` instance of `Builder`, then we always get the efficient
-- | `snoc` case.
-- |
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

infixl 5 append as <>>

instance monoidBuilder :: Monoid Builder where
  mempty = Null

-- | Stack-safe `foldl` over a `Builder`. *O(n)*
foldl :: forall a. (a -> DataBuff -> a) -> a -> Builder -> a
foldl f a0 b = (unwrap :: Identity a -> a) $ foldM (\a' b' -> pure (f a' b')) a0 b

-- | Stack-safe `foldM` over a `Builder`. *O(n)*.
foldM :: forall m a. MonadRec m => (a -> DataBuff -> m a) -> a -> Builder -> m a
-- The Art of Computer Programming 2.3.1
-- Inorder traversal using a stack
-- https://yuyuan.org/MorrisAlgorithm/
foldM f a0 b = tailRecM outer { p: b, stack: Nil, accum: a0 }
  where
  outer { p, stack, accum } = do
    stack' <- tailRecM inner { p_: p, stack_: stack }
    case uncons stack' of
      Nothing -> pure $ Done accum
      Just { head: Node _ x r, tail } -> do
        accum' <- f accum x
        pure $ Loop { p: r, stack: tail, accum: accum' }
      Just { head: Null } -> pure $ Done accum
  inner { p_: Null, stack_ } = pure $ Done stack_
  inner { p_: n@(Node l _ _), stack_ } = pure $ Loop { p_: l, stack_: n : stack_ }

-- | Construct a `Builder` with a single `DataBuff`. *O(1)*
singleton :: DataBuff -> Builder
singleton buf = Node Null buf Null

-- | Calculate the total byte length of the `Builder`, without actually
-- | building it yet. *O(n)*
length :: Builder -> ByteLength
length bldr = foldl (\b a -> b + len a) 0 bldr
  where
  len (Buff ab) = AB.byteLength ab
  len (View dv) = DV.byteLength dv

-- | Prepend a `DataBuff` to the beginning of the `Builder`. *O(1)*
cons :: DataBuff -> Builder -> Builder
cons x bs = Node Null x bs

-- | Append a `DataBuff` to the end of the `Builder`. *O(1)*
snoc :: Builder -> DataBuff -> Builder
snoc bs x = Node bs x Null

-- | Build a single `ArrayBuffer` from a `Builder`. *O(n)*
execBuilder :: forall m. MonadEffect m => MonadRec m => Builder -> m ArrayBuffer
execBuilder bldr = do
  let buflen = length bldr
  -- Allocate the final ArrayBuffer
  buf <- liftEffect $ AB.empty buflen
  -- Then copy each ArrayBuffer into the final ArrayBuffer.
  -- To memcpy from one ArrayBuffer to another, apparently we have to first view
  -- both ArrayBuffers as ArrayView (Typed Array).
  newview <- liftEffect (AT.whole buf :: Effect Uint8Array)
  _ <- foldM
    ( \offset a -> do
        aview <- liftEffect $ toUint8Array a
        _ <- liftEffect $ AT.setTyped newview (Just offset) aview
        pure $ offset + AT.byteLength aview
    )
    0
    bldr
  pure buf
  where
  toUint8Array :: DataBuff -> Effect Uint8Array
  toUint8Array (Buff ab) = AT.whole ab
  toUint8Array (View dv) =
    AT.part (DV.buffer dv) (DV.byteOffset dv) (DV.byteLength dv)

-- | Serialize an 8-bit unsigned integer (byte) into a new `ArrayBuffer`.
encodeUint8 :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint8 x = do
  buf <- liftEffect $ AB.empty 1
  _ <- liftEffect $ DV.setUint8 (DV.whole buf) 0 x
  pure buf

-- | Serialize an 8-bit two’s-complement signed integer (char) into a new `ArrayBuffer`.
encodeInt8 :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt8 x = do
  buf <- liftEffect $ AB.empty 1
  _ <- liftEffect $ DV.setInt8 (DV.whole buf) 0 x
  pure buf

-- | Serialize a 16-bit big-endian unsigned integer into a new `ArrayBuffer`.
encodeUint16be :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint16be x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setUint16be (DV.whole buf) 0 x
  pure buf

-- | Serialize a 16-bit little-endian unsigned integer into a new `ArrayBuffer`.
encodeUint16le :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint16le x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setUint16le (DV.whole buf) 0 x
  pure buf

-- | Serialize a 16-bit big-endian two’s-complement signed integer into a new `ArrayBuffer`.
encodeInt16be :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt16be x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setInt16be (DV.whole buf) 0 x
  pure buf

-- | Serialize a 16-bit little-endian two’s-complement signed integer into a new `ArrayBuffer`.
encodeInt16le :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt16le x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setInt16le (DV.whole buf) 0 x
  pure buf

-- | Serialize a 32-bit big-endian unsigned integer into a new `ArrayBuffer`.
encodeUint32be :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint32be x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setUint32be (DV.whole buf) 0 x
  pure buf

-- | Serialize a 32-bit little-endian unsigned integer into a new `ArrayBuffer`.
encodeUint32le :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint32le x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setUint32le (DV.whole buf) 0 x
  pure buf

-- | Serialize a 32-bit big-endian two’s-complement signed integer into a new `ArrayBuffer`.
encodeInt32be :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt32be x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setInt32be (DV.whole buf) 0 x
  pure buf

-- | Serialize a 32-bit little-endian two’s-complement signed integer into a new `ArrayBuffer`.
encodeInt32le :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt32le x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setInt32le (DV.whole buf) 0 x
  pure buf

-- | Serialize a 32-bit big-endian IEEE single-precision float into a new `ArrayBuffer`.
encodeFloat32be :: forall m. (MonadEffect m) => Float32 -> m ArrayBuffer
encodeFloat32be x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setFloat32be (DV.whole buf) 0 x
  pure buf

-- | Serialize a 32-bit little-endian IEEE single-precision float into a new `ArrayBuffer`.
encodeFloat32le :: forall m. (MonadEffect m) => Float32 -> m ArrayBuffer
encodeFloat32le x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setFloat32le (DV.whole buf) 0 x
  pure buf

-- | Serialize a 64-bit big-endian IEEE double-precision float into a new `ArrayBuffer`.
encodeFloat64be :: forall m. (MonadEffect m) => Number -> m ArrayBuffer
encodeFloat64be x = do
  buf <- liftEffect $ AB.empty 8
  _ <- liftEffect $ DV.setFloat64be (DV.whole buf) 0 x
  pure buf

-- | Serialize a 64-bit little-endian IEEE double-precision float into a new `ArrayBuffer`.
encodeFloat64le :: forall m. (MonadEffect m) => Number -> m ArrayBuffer
encodeFloat64le x = do
  buf <- liftEffect $ AB.empty 8
  _ <- liftEffect $ DV.setFloat64le (DV.whole buf) 0 x
  pure buf
