-- | This module provides a `Builder` monoid and a `PutM` monad
-- | for serializing `Data.ArrayBuffer.Types.ArrayBuffer`s.
-- | See the package README for usage examples.
-- |
-- | Writing to an `ArrayBuffer` is an `Effect`ful activity, so most
-- | functions in this module must be run in a `MonadEffect` context.
-- |
-- | For other operations for working with `ArrayBuffer`, see
-- | module
-- | [`Data.ArrayBuffer.ArrayBuffer`](https://pursuit.purescript.org/packages/purescript-arraybuffer/docs/Data.ArrayBuffer.ArrayBuffer)
-- | in package __arraybuffer__.
module Data.ArrayBuffer.Builder
  ( PutM
  , Put
  , execPutM
  , execPut
  , subBuilder
  , putArrayBuffer
  , putDataView
  , putDataBuff
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
  , module Data.ArrayBuffer.Builder.Internal
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell, lift)
import Data.ArrayBuffer.Builder.Internal (Builder, DataBuff(..), length, toView, (<>>))
import Data.ArrayBuffer.Builder.Internal as Internal
import Data.ArrayBuffer.Types (ArrayBuffer, DataView)
import Data.Float32 (Float32)
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Class (class MonadEffect)

-- | The `PutM` monad is a `WriterT Builder` transformer monad which
-- | gives us do-notation for the `Builder` monoid. The base monad must be
-- | a `MonadEffect`.
-- |
-- | To append `Builder`s in this monad call `tell`, or any of the `put*`
-- | functions in this module.
type PutM = WriterT Builder

-- | The `PutM` type reified to `Effect`, in other words `WriterT Builder Effect`.
type Put = PutM Effect

-- | Build an `ArrayBuffer` with do-notation in any `MonadEffect`. *O(n)* Stack-safe.
execPutM :: forall m. MonadEffect m => MonadRec m => PutM m Unit -> m ArrayBuffer
execPutM = Internal.execBuilder <=< execWriterT

-- | Build an `ArrayBuffer` with do-notation in `Effect`. *O(n)* Stack-safe.
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
-- | ```
-- | do
-- |   messageBuilder <- subBuilder $ do
-- |     putField1
-- |     putField2
-- |
-- |   putInt32be $ length messageBuilder
-- |   tell messageBuilder
-- | ```
subBuilder :: forall m. Monad m => PutM m Unit -> PutM m Builder
subBuilder = lift <<< execWriterT

-- | Append an `ArrayBuffer` to the builder.
putArrayBuffer :: forall m. Monad m => ArrayBuffer -> PutM m Unit
putArrayBuffer = tell <<< Internal.singleton <<< Internal.Buff

-- | Append a `DataView` to the builder.
putDataView :: forall m. Monad m => DataView -> PutM m Unit
putDataView = tell <<< Internal.singleton <<< Internal.View

-- | Append either an `ArrayBuffer` or a `DataView` to the builder.
putDataBuff :: forall m. Monad m => DataBuff -> PutM m Unit
putDataBuff = tell <<< Internal.singleton

-- | Append an 8-bit unsigned integer (byte) to the builder.
putUint8 :: forall m. MonadEffect m => UInt -> PutM m Unit
putUint8 = putArrayBuffer <=< Internal.encodeUint8

-- | Append an 8-bit two’s-complement signed integer (char) to the builder.
putInt8 :: forall m. MonadEffect m => Int -> PutM m Unit
putInt8 = putArrayBuffer <=< Internal.encodeInt8

-- | Append a 16-bit big-endian unsigned integer to the builder.
putUint16be :: forall m. MonadEffect m => UInt -> PutM m Unit
putUint16be = putArrayBuffer <=< Internal.encodeUint16be

-- | Append a 16-bit little-endian unsigned integer to the builder.
putUint16le :: forall m. MonadEffect m => UInt -> PutM m Unit
putUint16le = putArrayBuffer <=< Internal.encodeUint16le

-- | Append a 16-bit big-endian two’s-complement signed integer to the builder.
putInt16be :: forall m. MonadEffect m => Int -> PutM m Unit
putInt16be = putArrayBuffer <=< Internal.encodeInt16be

-- | Append a 16-bit little-endian two’s-complement signed integer to the builder.
putInt16le :: forall m. MonadEffect m => Int -> PutM m Unit
putInt16le = putArrayBuffer <=< Internal.encodeInt16le

-- | Append a 32-bit big-endian unsigned integer to the builder.
putUint32be :: forall m. MonadEffect m => UInt -> PutM m Unit
putUint32be = putArrayBuffer <=< Internal.encodeUint32be

-- | Append a 32-bit little-endian unsigned integer to the builder.
putUint32le :: forall m. MonadEffect m => UInt -> PutM m Unit
putUint32le = putArrayBuffer <=< Internal.encodeUint32le

-- | Append a 32-bit big-endian two’s-complement signed integer to the builder.
putInt32be :: forall m. MonadEffect m => Int -> PutM m Unit
putInt32be = putArrayBuffer <=< Internal.encodeInt32be

-- | Append a 32-bit little-endian two’s-complement signed integer to the builder.
putInt32le :: forall m. MonadEffect m => Int -> PutM m Unit
putInt32le = putArrayBuffer <=< Internal.encodeInt32le

-- | Append a 32-bit big-endian IEEE single-precision float to the builder.
putFloat32be :: forall m. MonadEffect m => Float32 -> PutM m Unit
putFloat32be = putArrayBuffer <=< Internal.encodeFloat32be

-- | Append a 32-bit little-endian IEEE single-precision float to the builder.
putFloat32le :: forall m. MonadEffect m => Float32 -> PutM m Unit
putFloat32le = putArrayBuffer <=< Internal.encodeFloat32le

-- | Append a 64-bit big-endian IEEE double-precision float to the builder.
putFloat64be :: forall m. MonadEffect m => Number -> PutM m Unit
putFloat64be = putArrayBuffer <=< Internal.encodeFloat64be

-- | Append a 64-bit little-endian IEEE double-precision float to the builder.
putFloat64le :: forall m. MonadEffect m => Number -> PutM m Unit
putFloat64le = putArrayBuffer <=< Internal.encodeFloat64le
