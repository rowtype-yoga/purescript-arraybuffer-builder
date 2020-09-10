## v1.1.0 2020-09-10

Add and export

* `cons :: ArrayBuffer -> Builder -> Builder`
* `subBuilder :: forall m. (MonadEffect m) => PutM m Unit -> PutM m Builder`
* `length :: Builder -> Int`
