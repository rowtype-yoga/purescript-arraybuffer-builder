# purescript-arraybuffer-builder

`ArrayBuffer` serialization in the “builder monoid” and “builder monad” style.
In this style, we build up serialized data structures by `append`ing to
a monoid in a Writer monad with do-notation. This style of serialization
has been used for a long time and we [insist that it works really well]
(https://wiki.haskell.org/Do_notation_considered_harmful#Library_design).

This package provides a `Builder` monoid and a `PutM` monad which are roughly
equivalent to types of the same name in the Haskell
[`Data.Binary.Put`](https://hackage.haskell.org/package/binary/docs/Data-Binary-Put.html)
module.

This package defines no typeclasses. Typeclass-based
serialization/deserialization generally assumes that we are round-tripping
some data structures out to storage and then back into our program.
This package is designed for the case in which we are serializing some data
to be sent to another program which expects a serialization format
that we don't control.

## Deserialization

This package is only for writing `ArrayBuffer`s, not reading them.
See
[__purescript-parsing-dataview__](https://pursuit.purescript.org/packages/purescript-parsing-dataview/)
for a way to deserialize from `ArrayBuffer` back to Purescript data.

## Usage Examples

All `ArrayBuffer` building must occur in `Effect`.

### Serialize an integer

Create a two-byte `arraybuffer :: ArrayBuffer` which contains the number *-2* encoded as big-endian 16-bit two’s-complement.
```purescript
do
  arraybuffer <- execPut $ putInt16be (-2)
```

### Serialize a `String` as UTF8

A function which encodes a `String` as UTF8 with a length prefix in a
way that's compatible with the
[`Binary.Put.putStringUtf8`](https://hackage.haskell.org/package/binary/docs/Data-Binary-Put.html#v:putStringUtf8)
function from the Haskell
[__binary__](https://hackage.haskell.org/package/binary)
library.
Uses [`Data.TextEncoding.encodeUtf8`](https://pursuit.purescript.org/packages/purescript-text-encoding/docs/Data.TextEncoding#v:encodeUtf8).
```purescript
putStringUtf8 :: forall m. (MonadEffect m) => String -> PutM m Unit
putStringUtf8 s = do
    let stringbuf = Data.ArrayBuffer.Typed.buffer $
            Data.TextEncoding.encodeUtf8 s
    -- Put a 64-bit big-endian length prefix for the length of the utf8
    -- string, in bytes.
    putUint32be 0
    putUint32be $ Data.Uint.fromInt $ Data.ArrayBuffer.byteLength stringbuf
    putArrayBuffer stringbuf
```

### Serialize an `Array Int`

A function which encodes an `Array Int` with a length prefix in a
way that's compatible with the
[`Binary` instance for `[Int32]`](https://hackage.haskell.org/package/binary/docs/Data-Binary.html#t:Binary)
from the Haskell
[__binary__](https://hackage.haskell.org/package/binary)
library.
```purescript
putArrayInt32 :: forall m. (MonadEffect m) => Array Int -> PutM m Unit
putArrayInt32 xs = do
    -- Put a 64-bit big-endian length prefix for the length of the
    -- array, in bytes.
    putUint32be 0
    putUint32be $ Data.Uint.fromInt $ Data.Array.length xs
    traverse_ putInt32be xs
```
