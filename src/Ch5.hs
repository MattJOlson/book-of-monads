module Ch5 where

-- yum, laws

-- Functor laws:
-- fmapping id is id
-- fmap id == id
--
-- fmapping a composed function is composing fmapped functions
-- fmap (f . g) == fmap f . fmap g
--
-- taken together, these say that fmapping only changes the elements in the
-- functor, not the shape of the functor (i.e. it doesn't reorder or truncate
-- lists, drop data from maybes, etc)

-- Monoid laws:
-- bidirectional identity
-- mempty <> a == a <> mempty == a
--
-- associativity
-- a <> (b <> c) == (a <> b) <> c

-- 5.1. Describe two monoids on Maybe (hint: "left-biased" and "right-biased")
--
-- "left/right biased" makes me think of left and right identities, that's wrong
-- because monoids have a left+right identity but let's work it out
--
-- here's one attempt, however this fails the first law - Nothing annihilates
-- values from the left, and there's no good candidate for an identity on Maybe
-- other than Nothing
mappendLM :: Maybe a -> Maybe a -> Maybe a
mappendLM a b = case (a, b) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just _) -> Nothing
  (Just a, Nothing) -> Just a
  (Just a, Just _) -> Just a

-- here's a better version, note that the left bias shows up in the case where
-- you have two non-identity elements and need to pick one ("pick" because it's
-- a monoid on Maybe, not a monoid on a)
mappendLM' :: Maybe a -> Maybe a -> Maybe a
mappendLM' a b = case (a, b) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just b) -> Just b
  (Just a, Nothing) -> Just a
  (Just a, Just _) -> Just a

-- and the obvious right-biased version
mappendRM :: Maybe a -> Maybe a -> Maybe a
mappendRM a b = case (a, b) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just b) -> Just b
  (Just a, Nothing) -> Just a
  (Just _, Just b) -> Just b

-- a function is a monoid homomorphism if it preserves monoidal structure, which
-- is similar to fmap. given a function f, f is a monoid homomorphism iff
--
-- f id == id // for monoidal identity, not the `id` function
-- f (x <> y) == (f x) <> (f y)

-- 5.2. Show that exp x = e^x is a monoid homomorphism between real addition and
-- real multiplication
-- (the book says "numbers" but I an't proving this on C)
--
-- x :: "addition" numbers, e^x :: "multiplication" numbers
--
-- identity: the additive identity is 0, so f id = e^0 = 1, which is the
-- multiplicative identity
--
-- composition:
-- f (x <> y) = e^(x + y) = e^x * e^y = (f x) <> (f y)
-- note that <> on the left side means +, and <> on the right side means *

-- monad laws, here we go baby
--
-- the book uses `return` but I'm going to use `pure` because it's shorter
--
-- let's consider a monadic function f :: a -> m b
--
-- first a left identity law:
-- pure x >>= f == f x
--
-- that is, I can either run f x and get some m b, or I can pure x into
-- m x and bind it to f, and still get an m b. pure is my identity and >>= is a
-- binary operator.
--
-- right identity:
-- m b == m b >>= pure
--
-- that is, if I stick pure on the right side of the >>=, it also acts as an
-- identity, even though the left-identity law talks about f (our monadic
-- function) and the right-identity law talks about m (the monad f operates on)
--
-- associativity:
-- (m b >>= f) >>= g == m b >>= (\x -> f x >>= g)
--
-- this one's a bit weird because of the lambda, but since we're writing these
-- in pseudo-haskell we need a way to run the LHS of the second >>= on x, not
-- the RHS. anyway, it's a construction of associativity on bind and return, and
-- I'm sure we'll get into composability of monads later in the book.
--
-- this looks familiar, right? so now we have a monoid, we just need to figure
-- out what a category of endofunctors is and we'll understand monads. (lol)
--
-- the consequences of this are neat:
-- - the identity laws show that lawful monads don't mess with pure values; we
--   can run monads on them all we want and not fuck 'em up
-- - the associativity law powers do notation; we can write
--   do x <- foo
--      y <- bar x
--      g y
--   and not worry about how the syntax sugar-er "parenthesizes" those lines

-- ope, spoiler warning, we're already on Kleisli arrows
-- so a Kleisli arrow is a function into a monad, like f :: a -> m b
-- we can compose those with a "Kleisli composition" fn, named <=< because
-- Haskell flows to the left and makes me sad
--
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (<=<) :: (b -> m c) -> (a -> m b) -> (a -> m c)
--
-- as an aside, I hope I'll remember this when I get to contravariant functors
-- again
--
-- so now our monad laws look like monoid laws:
--
-- bind <=< f == f                    -- left identity
-- f == f <=< bind                    -- right identity
-- (f <=< g) <=< h == f <=< (g <=< h) -- associativity