# Changelog

## v3.0.1 2022-05-04

CI repair. Change `test.dhall` to `spago-dev.dhall`.

## v3.0.0 2022-05-04

Build for PureScript v0.15.

Added `MonadRec` constraint to `execPut`, `execPutM`, `foldl`, `foldM`,
they are now stack-safe.

## v2.1.0 2021-07-22

Renamed `Data.ArrayBuffer.Internal.Bytes` and exported
it as `Data.ArrayBuffer.DataBuff`.

## v2.0.0 2021-07-19

Build for PureScript v0.14.

### Breaking Changes

Previously we could only add `ArrayBuffer`s to the `Builder`,
and now we can add either `ArrayBuffer` or `DataView` to the `Builder`,
because we have added a `Bytes` type which distinguishes between `ArrayBuffer`
and `DataView`. Because of this, we can avoid making an
intermediate `ArrayBuffer` copy when adding a slice of an existing `ArrayBuffer`.

Factored out the `Internal` module and added documentation.

## v1.1.0 2020-09-10

Add and export

* `cons :: ArrayBuffer -> Builder -> Builder`
* `subBuilder :: forall m. (MonadEffect m) => PutM m Unit -> PutM m Builder`
* `length :: Builder -> Int`
