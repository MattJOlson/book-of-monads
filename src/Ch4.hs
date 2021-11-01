module Ch4 where

-- working over a list with monadic effects, this doesn't typecheck:
-- 
-- foo :: Monad m => (a -> m b) -> [a] -> m [b]
-- foo = map 
--
-- Expected type: (a -> m b) -> [a] -> m [b]
--   Actual type: (a -> [b]) -> [a] -> [[b]]
--
-- because the typechecker narrows m down to []
--
-- fortunately, we have mapM, which has exactly the type signature of foo in
-- GHC.Base but in Prelude is defined on Traversable instead of []
--
-- speaking of traversable, there's also sequence and sequenceA

-- 4.1. implement:
-- zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
-- replicateM :: Monad m => Int -> m a -> m [a]
-- 
-- in terms of zipWith and replicate, try it with:
-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
--
-- and see what goes wrong
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f as bs = sequenceA $ zipWith f as bs

-- well that suggests an obvious implementation for replicateM
replicateM :: Monad m => Int -> m a -> m [a]
replicateM ct atom = sequenceA $ replicate ct atom

-- okay so...
-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- filterM pred as = sequenceA $ filter pred as
--
-- this function body has type Applicative f => (f a -> Bool) -> [f a] -> f [a]
-- the problem is that the predicate is monadic in its second arg, not its first
-- 
-- oops, I was supposed to use squence, not sequenceA. whatever

-- okay, we're talking about discarding values and foobarM_ functions
-- apparently _ <- blah is Considered Harmful, what if you ignore something that
-- isn't a ()? (well, maybe you meant to). just `blah` is fine in a do block if
-- you want to discard a () though
--
-- somehow I never twigged that () is an empty tuple

-- some neat stuff about the monad-loops package

-- traverse / traversable
-- traverse is to mapM what fmap is to map, or something
-- "learn to spot places where a container - a Traversable - is used in a
-- monadic context, sequenceA and traverse will come in handy"
-- (sequence is just sequenceA but requiring a monad rather than an applicative)

-- sequence [Maybe a] :: Maybe [a]
-- this looks like the (,) <*> validateFoo <$> validateBar example, doesn't it?

-- the Identity monad, and in particular how mapM Identity is just fmap