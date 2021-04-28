-- | This module provides a `Builder` monoid and a `PutM` monad
-- | for serializing `Data.ArrayBuffer.Types.ArrayBuffer`s.
-- | See the package README for usage examples.
-- |
-- | Writing to an `ArrayBuffer` is an `Effect`ful activity, so most
-- | functions in this module must be run in a `MonadEffect` context.
-- |
-- | For operations for working with `ArrayBuffer`, see
-- | module
-- | [`Data.ArrayBuffer.ArrayBuffer`](https://pursuit.purescript.org/packages/purescript-arraybuffer/docs/Data.ArrayBuffer.ArrayBuffer)
-- | in package __purescript-arraybuffer__.
module Data.ArrayBuffer.Builder
( PutM
, Put
, execPutM
, execPut
, subBuilder
, putArrayBuffer
, putDataView
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
, Builder
, (<>>)
, Bytes
, toView
, execBuilder
, length
, singleton
, singleton_
, cons
, cons_
, snoc
, snoc_
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
)
where

import Prelude

import Control.Monad.Writer.Trans (WriterT, execWriterT, tell, lift)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array, DataView, ByteLength)
import Data.Float32 (Float32)
import Data.Maybe (Maybe(..))
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

-- | The `PutM` monad is a `WriterT Builder` transformer monad which
-- | gives us do-notation for the `Builder` monoid. The base monad must be
-- | a `MonadEffect`.
-- |
-- | To append `Builder`s in this monad call `tell`, or any of the `put*`
-- | functions in this module.
type PutM = WriterT Builder

-- | The `PutM` type reified to `Effect`, in other words `WriterT Builder Effect`.
type Put = PutM Effect

-- | Monoidal builder for `ArrayBuffer`s.
-- |
-- | We can add two types of things to the `Builder`: `ArrayBuffer`s and
-- | `DataView`s.
-- | We might prefer
-- | to add a `DataView` to a `Builder` when we’re adding a large slice of data
-- | from some other `ArrayBuffer`, so that we don’t
-- | need an extra intermediate copy of the slice.
-- | Our convention is that functions which add `DataView`s to
-- | the `Builder` are suffixed with underscore (`_`).
-- |
-- | ### Left-associative `<>>` append operator
-- |
-- | __TL;DR__ You probably don't want to use the `Builder` monoid directly
-- | in your code, it’s better to use the `PutM` monad with do-notation instead.
-- |
-- | The `Builder` monoid in this library is efficient when we `snoc` single
-- | items onto the end of it, or when we only `cons` single items to the
-- | beginning, but it can be less efficient when we are mixing `cons` and
-- | `snoc`.
-- | This matters when we're appending three or more `Builder`s
-- | in one associative expression, because the
-- | `Semigroup` append operator `<>` is right-associative.
-- |
-- | To solve this, we provide an operator `<>>` for appending `Builders`.
-- | `<>>` is exactly the same as `<>`, but left-associative.
-- | This only matters when we're chaining together three
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
data Builder
  = Node Builder Bytes Builder
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

infixl 5 append as <>>

instance monoidBuilder :: Monoid Builder where
  mempty = Null

-- | For distinguishing between `ArrayBuffer` and  `DataView`.
data Bytes
  = ByteBuff ArrayBuffer
  | ByteView DataView

toView :: Bytes -> DataView
toView (ByteBuff ab) = DV.whole ab
toView (ByteView dv) = dv

-- | `Builder` is not an instance of `Foldable` because `Builder` is monomorphic.
foldl :: forall b. (b -> Bytes -> b) -> b -> Builder -> b
foldl _ a Null = a
foldl f a (Node l x r) = foldl f (f (foldl f a l) x) r

-- | Monomorphic foldM copied from
-- | [`Data.Foldable.foldM`](https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Foldable#v:foldM)
foldM :: forall b m. (Monad m) => (b -> Bytes -> m b) -> b -> Builder -> m b
foldM f a0 = foldl (\ma b -> ma >>= flip f b) (pure a0)

-- | Construct a `Builder` with a single `ArrayBuffer`. *O(1)*
singleton :: ArrayBuffer -> Builder
singleton buf = Node Null (ByteBuff buf) Null

-- | Construct a `Builder` with a single `DataView`. *O(1)*
singleton_ :: DataView -> Builder
singleton_ buf = Node Null (ByteView buf) Null

-- | Prepend an `ArrayBuffer` to the beginning of the `Builder`. *O(1)*
cons :: ArrayBuffer -> Builder -> Builder
cons x bs = Node Null (ByteBuff x) bs

-- | Prepend a `DataView` to the beginning of the `Builder`. *O(1)*
cons_ :: DataView -> Builder -> Builder
cons_ x bs = Node Null (ByteView x) bs

-- | Append an `ArrayBuffer` to the end of the `Builder`. *O(1)*
snoc :: Builder -> ArrayBuffer -> Builder
snoc bs x = Node bs (ByteBuff x) Null

-- | Append a `DataView` to the end of the `Builder`. *O(1)*
snoc_ :: Builder -> DataView -> Builder
snoc_ bs x = Node bs (ByteView x) Null

-- | Calculate the total byte length of the `Builder`. *O(n)*
length :: Builder -> ByteLength
length bldr = foldl (\b a -> b + len a) 0 bldr
  where
    len (ByteBuff ab) = AB.byteLength ab
    len (ByteView dv) = DV.byteLength dv

-- | Build a single `ArrayBuffer` from a `Builder`. *O(n)*
execBuilder :: forall m. (MonadEffect m) => Builder -> m ArrayBuffer
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
    ) 0 bldr
  pure buf
 where
  toUint8Array :: Bytes -> Effect Uint8Array
  toUint8Array (ByteBuff ab) = AT.whole ab
  toUint8Array (ByteView dv) =
    AT.part (DV.buffer dv) (DV.byteOffset dv) (DV.byteLength dv)

-- | Build an `ArrayBuffer` with do-notation in any `MonadEffect`. *O(n)*
execPutM :: forall m. (MonadEffect m) => PutM m Unit -> m ArrayBuffer
execPutM = execBuilder <=< execWriterT

-- | Build an `ArrayBuffer` with do-notation. *O(n)*
execPut :: Put Unit -> Effect ArrayBuffer
execPut = execPutM

-- | Build up a sub-`Builder` without `tell`ing it to the `Writer` yet.
-- |
-- | One case where we might want to call a `subBuilder` is when
-- | serializing length-prefixed messages in some protocol. In that case,
-- | we must serialize the message first, calculate the message length,
-- | append the message length, and then append the message.
-- |
-- | In a `PutM` monad do-block, we can
-- |
-- |
-- | ```
-- | do
-- |   messageBuilder <- subBuilder $ do
-- |     putField1
-- |     putField2
-- |
-- |   putInt32be $ length messageBuilder
-- |   tell messageBuilder
-- | ```
subBuilder :: forall m. (MonadEffect m) => PutM m Unit -> PutM m Builder
subBuilder = lift <<< execWriterT

-- | Append an `ArrayBuffer` to the builder.
putArrayBuffer :: forall m. (MonadEffect m) => ArrayBuffer -> PutM m Unit
putArrayBuffer = tell <<< singleton

-- | Append a `DataView` to the builder.
putDataView :: forall m. (MonadEffect m) => DataView -> PutM m Unit
putDataView = tell <<< singleton_

-- | Serialize an 8-bit unsigned integer (byte) into a new `ArrayBuffer`.
encodeUint8 :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint8 x = do
  buf <- liftEffect $ AB.empty 1
  _ <- liftEffect $ DV.setUint8 (DV.whole buf) 0 x
  pure buf

-- | Append an 8-bit unsigned integer (byte) to the builder.
putUint8 :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint8 = putArrayBuffer <=< encodeUint8

-- | Serialize an 8-bit two’s-complement signed integer (char) into a new `ArrayBuffer`.
encodeInt8 :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt8 x = do
  buf <- liftEffect $ AB.empty 1
  _ <- liftEffect $ DV.setInt8 (DV.whole buf) 0 x
  pure buf

-- | Append an 8-bit two’s-complement signed integer (char) to the builder.
putInt8 :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt8 = putArrayBuffer <=< encodeInt8

-- | Serialize a 16-bit big-endian unsigned integer into a new `ArrayBuffer`.
encodeUint16be :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint16be x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setUint16be (DV.whole buf) 0 x
  pure buf

-- | Append a 16-bit big-endian unsigned integer to the builder.
putUint16be :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint16be = putArrayBuffer <=< encodeUint16be

-- | Serialize a 16-bit little-endian unsigned integer into a new `ArrayBuffer`.
encodeUint16le :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint16le x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setUint16le (DV.whole buf) 0 x
  pure buf

-- | Append a 16-bit little-endian unsigned integer to the builder.
putUint16le :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint16le = putArrayBuffer <=< encodeUint16le

-- | Serialize a 16-bit big-endian two’s-complement signed integer into a new `ArrayBuffer`.
encodeInt16be :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt16be x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setInt16be (DV.whole buf) 0 x
  pure buf

-- | Append a 16-bit big-endian two’s-complement signed integer to the builder.
putInt16be :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt16be = putArrayBuffer <=< encodeInt16be

-- | Serialize a 16-bit little-endian two’s-complement signed integer into a new `ArrayBuffer`.
encodeInt16le :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt16le x = do
  buf <- liftEffect $ AB.empty 2
  _ <- liftEffect $ DV.setInt16le (DV.whole buf) 0 x
  pure buf

-- | Append a 16-bit little-endian two’s-complement signed integer to the builder.
putInt16le :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt16le = putArrayBuffer <=< encodeInt16le

-- | Serialize a 32-bit big-endian unsigned integer into a new `ArrayBuffer`.
encodeUint32be :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint32be x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setUint32be (DV.whole buf) 0 x
  pure buf

-- | Append a 32-bit big-endian unsigned integer to the builder.
putUint32be :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint32be = putArrayBuffer <=< encodeUint32be

-- | Serialize a 32-bit little-endian unsigned integer into a new `ArrayBuffer`.
encodeUint32le :: forall m. (MonadEffect m) => UInt -> m ArrayBuffer
encodeUint32le x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setUint32le (DV.whole buf) 0 x
  pure buf

-- | Append a 32-bit little-endian unsigned integer to the builder.
putUint32le :: forall m. (MonadEffect m) => UInt -> PutM m Unit
putUint32le = putArrayBuffer <=< encodeUint32le

-- | Serialize a 32-bit big-endian two’s-complement signed integer into a new `ArrayBuffer`.
encodeInt32be :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt32be x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setInt32be (DV.whole buf) 0 x
  pure buf

-- | Append a 32-bit big-endian two’s-complement signed integer to the builder.
putInt32be :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt32be = putArrayBuffer <=< encodeInt32be

-- | Serialize a 32-bit little-endian two’s-complement signed integer into a new `ArrayBuffer`.
encodeInt32le :: forall m. (MonadEffect m) => Int -> m ArrayBuffer
encodeInt32le x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setInt32le (DV.whole buf) 0 x
  pure buf

-- | Append a 32-bit little-endian two’s-complement signed integer to the builder.
putInt32le :: forall m. (MonadEffect m) => Int -> PutM m Unit
putInt32le = putArrayBuffer <=< encodeInt32le

-- | Serialize a 32-bit big-endian IEEE single-precision float into a new `ArrayBuffer`.
encodeFloat32be :: forall m. (MonadEffect m) => Float32 -> m ArrayBuffer
encodeFloat32be x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setFloat32be (DV.whole buf) 0 x
  pure buf

-- | Append a 32-bit big-endian IEEE single-precision float to the builder.
putFloat32be :: forall m. (MonadEffect m) => Float32 -> PutM m Unit
putFloat32be = putArrayBuffer <=< encodeFloat32be

-- | Serialize a 32-bit little-endian IEEE single-precision float into a new `ArrayBuffer`.
encodeFloat32le :: forall m. (MonadEffect m) => Float32 -> m ArrayBuffer
encodeFloat32le x = do
  buf <- liftEffect $ AB.empty 4
  _ <- liftEffect $ DV.setFloat32le (DV.whole buf) 0 x
  pure buf

-- | Append a 32-bit little-endian IEEE single-precision float to the builder.
putFloat32le :: forall m. (MonadEffect m) => Float32 -> PutM m Unit
putFloat32le = putArrayBuffer <=< encodeFloat32le

-- | Serialize a 64-bit big-endian IEEE double-precision float into a new `ArrayBuffer`.
encodeFloat64be :: forall m. (MonadEffect m) => Number -> m ArrayBuffer
encodeFloat64be x = do
  buf <- liftEffect $ AB.empty 8
  _ <- liftEffect $ DV.setFloat64be (DV.whole buf) 0 x
  pure buf

-- | Append a 64-bit big-endian IEEE double-precision float to the builder.
putFloat64be :: forall m. (MonadEffect m) => Number -> PutM m Unit
putFloat64be = putArrayBuffer <=< encodeFloat64be

-- | Serialize a 64-bit little-endian IEEE double-precision float into a new `ArrayBuffer`.
encodeFloat64le :: forall m. (MonadEffect m) => Number -> m ArrayBuffer
encodeFloat64le x = do
  buf <- liftEffect $ AB.empty 8
  _ <- liftEffect $ DV.setFloat64le (DV.whole buf) 0 x
  pure buf

-- | Append a 64-bit little-endian IEEE double-precision float to the builder.
putFloat64le :: forall m. (MonadEffect m) => Number -> PutM m Unit
putFloat64le = putArrayBuffer <=< encodeFloat64le


-- ## Implementation Details
--
-- We want our `Builder` to be a data structure with
-- * *O(1)* monoid append
-- * *O(n)* fold
--
-- Our `Builder` implementation is an unbalanced binary tree.
--
-- For monoid `append`, what we actually get is *O(1)* when either the
-- left or right tree is a singleton. If that's not true, then in the
-- unlikely worst case `append` might be *O(n)*.
--
-- `Builder` is optimized for what we consider to be normal usage, that is,
-- `snoc`ing singleton elements to the end of the `Builder`.
--
-- If a Builder is built entirely by `snoc`ing, it will look like a
-- left-only binary tree, a.k.a. a linked list.
--
-- ```
--            ④
--           ╱
--          ③
--         ╱
--        ②
--       ╱
--      ①
-- ```
--
-- If two of these `snoc`-built trees are `append`ed, then the new tree
-- will look like
--
-- ```
--            ④
--           ╱  ╲
--          ③  ⑧
--         ╱   ╱
--        ②  ⑦
--       ╱   ╱
--      ①  ⑥
--         ╱
--        ⑤
-- ```
--
-- This is all similar to
-- [__bytestring-tree-builder__](https://hackage.haskell.org/package/bytestring-tree-builder)
-- , except that the
-- [`Tree`](https://hackage.haskell.org/package/bytestring-tree-builder-0.2.7.3/docs/src/ByteString.TreeBuilder.Tree.html#Tree)
-- structure in __bytestring-tree-builder__ only carries values in its
-- leaves, which is how it achieves *O(1)* appending, at the cost of
-- a higher coefficient time factor on the fold.
--
-- We hope that this implementation is fairly fast, but we
-- haven't chosen this implementation because it's fast, we've chosen
-- this implementation because it's simple.
-- If someone wants to create a fast Purescript ArrayBuffer serialization
-- library, then they can benchmark against this one to prove that the new
-- one is fast.
--
-- One relatively cheap and simple performance improvement for this library would be to
-- remove the Null constructor of `Builder` and instead use Javascript nulls.
-- UPDATE Actually no, we need the Null constructor for the Monoid instance.
--
-- In the longer term, it might make sense to try to change the Builder so
-- that it works like the
-- https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Builder.html
--
-- Here are some benchmarks of different Haskell ByteString builders
-- https://github.com/haskell-perf/strict-bytestring-builders
--
-- We've tried to design the API for this library with minimal assumptions,
-- so that if we want to change the Builder implementation later then we can.
--
-- Another way this could go:
-- https://kodimensional.dev/posts/2019-03-25-comonadic-builders

