# purescript-arraybuffer-builder

[![CI](https://github.com/jamesdbrock/purescript-arraybuffer-builder/workflows/CI/badge.svg?branch=master)](https://github.com/jamesdbrock/purescript-arraybuffer-builder/actions)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-arraybuffer-builder/badge)](http://pursuit.purescript.org/packages/purescript-arraybuffer-builder/)

`ArrayBuffer` serialization in the “builder monoid” and “builder monad” style.
In this style, we build up serialized data structures by `append`ing to
a monoid in a Writer monad with do-notation. This style of serialization
has been used for a long time and we [insist that it works really well](https://wiki.haskell.org/Do_notation_considered_harmful#Library_design).

This package provides a `Builder` monoid and a `PutM` monad which are roughly
equivalent to types of the same name in the Haskell
[`Data.Binary.Put`](https://hackage.haskell.org/package/binary/docs/Data-Binary-Put.html)
module.

This package defines
[no typeclasses](http://code.slipthrough.net/2018/03/13/thoughts-on-typeclass-codecs/).
Typeclass-based
serialization/deserialization generally assumes that we are round-tripping
some data structures out to storage and then back into our program.
This package is designed for the case in which we are serializing some data
to be sent to another program which expects a serialization format
that we don't control.

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
  arraybuffer :: ArrayBuffer <- execPut $ do
    putFloat64be 1.0
    putFloat64be 2.0
    putFloat64be 3.0
```

### Serialize a `String` as UTF8

Encode a `String` as UTF8 with a length prefix into our `Builder`.

We give this as an example, rather than supporting it in the library, because
it depends on
[`Data.TextEncoding.encodeUtf8`](https://pursuit.purescript.org/packages/purescript-text-encoding/docs/Data.TextEncoding#v:encodeUtf8).

```purescript
import Data.ArrayBuffer.Builder (PutM, putArrayBuffer, execPut)
import Data.ArrayBuffer.Typed (buffer)
import Data.TextEncoding (encodeUtf8)
import Data.ArrayBuffer.ArrayBuffer (byteLength)

putStringUtf8 :: forall m. (MonadEffect m) => String -> PutM m Unit
putStringUtf8 s = do
  let stringbuf = buffer $ encodeUtf8 s
  -- Put a 32-bit big-endian length prefix for the length of the utf8 string, in bytes.
  putInt32be $ byteLength stringbuf
  putArrayBuffer stringbuf

do
  arraybuffer :: ArrayBuffer <- execPut $ putStringUtf8 "BLM"
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

putArrayInt32 :: forall m. (MonadEffect m) => Array Int -> PutM m Unit
putArrayInt32 xs = do
    -- Put a 64-bit big-endian length prefix for the length of the array.
    putInt32be 0
    putInt32be $ length xs
    traverse_ putInt32be xs

do
  arraybuffer <- execPut $ putArrayInt32 [1,2,3]
```

## Deserialization

This package is only for writing `ArrayBuffer`s, not reading them.
See
[__purescript-parsing-dataview__](https://pursuit.purescript.org/packages/purescript-parsing-dataview/)
for a way to deserialize from `ArrayBuffer` back to Purescript data.

## References

* [MDN `ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)

## Alternative packages for serializing to an `ArrayBuffer`

* [__dynamic-buffers__](https://pursuit.purescript.org/packages/purescript-dynamic-buffers)
* [__node-buffer__](https://pursuit.purescript.org/packages/purescript-node-buffer)
* [__arraybuffer-class__](https://pursuit.purescript.org/packages/purescript-arraybuffer-class)
* [__bytestrings__](https://pursuit.purescript.org/packages/purescript-bytestrings/)