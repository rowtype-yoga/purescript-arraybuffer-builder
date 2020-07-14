module Builder
( Builder
)
where

import Data.ArrayBuffer.Types

type PutM m a = forall m a. (MonadEffect m) => WriterT Builder m a
type Put = PutM Effect Unit
 -- Note: use liftEffect to call the ArrayBuffer Effect functions

-- | Builder for `ArrayBuffer`s.
-- |
-- | It is common to build an `ArrayBuffer` by issuing a sequence of `tell`
-- | commands in a `WriterT Builder IO` monad.
-- |
-- | *O(1)* `mappend` and *O(1)* `snoc` and *O(n)* `fold`.
data Builder
  = Node ArrayBuffer Builder Builder
  | Null

instance semigroupBuilder :: Semigroup Builder where
  append Null Null = Null
  append l Null = l
  append Null r = r
  append (Node lx ll lr) (Node rx rl rr) = Node lx (Node )

-- data Builder
--   = Tree (Nullable Builder) (Nullable Builder)
--   | Leaf ArrayBuffer
--
-- instance semigroupBuilder :: Semigroup Builder where
--   append

data Builder
  = Tree Builder Builder
  | Snoc Builder ArrayBuffer
  | Nil

-- Note use `censor` instead of `tell` to accumulate, so we don't have to make
-- a dummy List and then strip it off.