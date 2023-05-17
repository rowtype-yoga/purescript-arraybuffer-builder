# purescript-arraybuffer-builder

[![CI](https://github.com/rowtype-yoga/purescript-arraybuffer-builder/workflows/CI/badge.svg?branch=master)](https://github.com/rowtype-yoga/purescript-arraybuffer-builder/actions)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-arraybuffer-builder/badge)](http://pursuit.purescript.org/packages/purescript-arraybuffer-builder/)
[![Maintainer: jamesdbrock](https://img.shields.io/badge/maintainer-jamesdbrock-teal.svg)](https://github.com/jamesdbrock)


[`ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
serialization in the “builder monoid” and “builder monad” style.
In this style, we build up serialized data structures by `tell`ing to
a `Writer` monad with `do`-notation. This style of serialization
has been used for a long time and we [insist that it works really well](https://wiki.haskell.org/Do_notation_considered_harmful#Library_design).

This package provides a `Builder` monoid and a `PutM` monad which are roughly
equivalent to types of the same name in the Haskell
[__Data.Binary.Put__](https://hackage.haskell.org/package/binary/docs/Data-Binary-Put.html)
module.

## Usage Examples

All `ArrayBuffer` building must occur in `Effect`.

### Serialize an integer

Create a two-byte `arraybuffer :: ArrayBuffer` which contains the number *-10* encoded as big-endian 16-bit two’s-complement.
```purescript
import Data.ArrayBuffer.Builder (execPut, putInt16be)

do
  arraybuffer :: ArrayBuffer <- execPut $ putInt16be (-10)
```

### Serialize three floats

Create a 24-byte `arraybuffer :: ArrayBuffer` which contains three big-endian
IEEE-754 double-precision floats.

```purescript
import Data.ArrayBuffer.Builder (execPut, putFloat64be)

do
  arraybuffer :: ArrayBuffer <- execPut do
    putFloat64be 1.0
    putFloat64be 2.0
    putFloat64be 3.0
```

### Serialize a `String` as UTF8

Encode a `String` as UTF8 with a length prefix into our `Builder`.

We give this as an example, rather than supporting it in the library, because
it depends on
[`Web.Encoding.TextEncoder`](https://pursuit.purescript.org/packages/purescript-web-encoding/docs/Web.Encoding.TextEncoder).

```purescript
import Effect.Class (class MonadEffect, liftEffect)
import Data.UInt (fromInt)
import Data.ArrayBuffer.Types (ArrayBuffer(..))
import Data.ArrayBuffer.Builder (PutM, putArrayBuffer, execPut, putUint32be)
import Data.ArrayBuffer.Typed (buffer)
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Web.Encoding.TextEncoder (new, TextEncoder, encode)

putStringUtf8 :: forall m. MonadEffect m => String -> PutM m Unit
putStringUtf8 s = do
  textEncoder <- liftEffect new
  let stringbuf = buffer $ encode s textEncoder
  -- Put a 32-bit big-endian length for the utf8 string, in bytes.
  putUint32be $ fromInt $ byteLength stringbuf
  putArrayBuffer stringbuf

arraybuffer :: ArrayBuffer <- execPut $ putStringUtf8 "🦝"
```

### Serialize an `Array Int`

Encode an `Array Int` with a length prefix in a
way that's compatible with the
[`Binary` instance for `[Int32]`](https://hackage.haskell.org/package/binary/docs/Data-Binary.html#t:Binary)
from the Haskell
[__binary__](https://hackage.haskell.org/package/binary)
library.

```purescript
import Data.ArrayBuffer.Builder (execPut, putInt32be)
import Data.Foldable (traverse_)
import Data.Array (length)

putArrayInt32 :: forall m. MonadEffect m => Array Int -> PutM m Unit
putArrayInt32 xs = do
    -- Put a 64-bit big-endian length prefix for the array.
    putInt32be 0
    putInt32be $ length xs
    traverse_ putInt32be xs

do
  arraybuffer <- execPut $ putArrayInt32 [1,2,3]
```

### Serialize an `Array Int` stack-safe

Stack-safe version of the `putArrayInt32` function above. For stack-safety
we use `foldRecM` instead of `traverse_` because `foldRecM` has a `MonadRec`
constraint which makes it stack-safe.

```purescript
import Data.Array (foldRecM)

putArrayInt32 :: forall m. MonadEffect m => MonadRec m => Array Int -> PutM m Unit
putArrayInt32 xs = do
    -- Put a 64-bit big-endian length prefix for the array.
    putInt32be 0
    putInt32be $ length xs
    foldRecM (\_ x -> putInt32be x) unit xs
```

## Stack-safety

This package will always be stack-safe if all of the functions called inside
of the `PutM` builder expression are stack-safe.

## Deserialization

This package is only for writing `ArrayBuffer`s, not reading them.
See
[__parsing-dataview__](https://pursuit.purescript.org/packages/purescript-parsing-dataview/)
for a way to deserialize from `ArrayBuffer` back to Purescript data.

## Alternative packages for serializing to an `ArrayBuffer`

* [__dynamic-buffers__](https://pursuit.purescript.org/packages/purescript-dynamic-buffers)
* [__node-buffer__](https://pursuit.purescript.org/packages/purescript-node-buffer)
* [__arraybuffer-class__](https://pursuit.purescript.org/packages/purescript-arraybuffer-class)
* [__bytestrings__](https://pursuit.purescript.org/packages/purescript-bytestrings/)
* [__array-builder__](https://pursuit.purescript.org/packages/purescript-array-builder)

## Development

To run the tests,

```shell
spago -x spago-dev.dhall test
```
