module Ch3 where

import Control.Applicative
import Control.Monad
import Data.Char

-- some interesting things going on with functors and lifting functions of
-- multiple arguments. if I have g :: a -> b -> c and fmap it over some m,
-- I end up with fmap g :: m a -> m (b -> c). now doesn't the latter look
-- familiar?

-- 3.1. implement ap
ap :: Monad m => m (b -> c) -> m b -> m c
ap mf mb = do
  f' <- mf
  b' <- mb
  pure $ f' b'

-- one definition of Applicative: it's a functorial context that supports
-- lifting any arity of functions, not just functions of one variable
-- if A is an applicative functor, then f :: a -> b -> c -> ... -> z can be
-- lifted into A f :: A a -> A b -> ... -> A z
-- this looks like f <$> a <*> b <*> ... <*> z

-- 3.2. implement fmap from pure and ap
fmap' :: Applicative m => (a -> b) -> m a -> m b
fmap' f ma = (pure f) <*> ma

-- lmao at hlint trying to get me to just use fmap in the above

-- 3.3. implement Functor for ZipList
-- well I'm just going to write an fmap for it... this is kind of silly, I
-- wrote map in ch1 and now I'm just fmapping over destructured types. oh well,
-- it's good to know about @ here
fmapZL :: (a -> b) -> ZipList a -> ZipList b
fmapZL f zla@(ZipList a) = ZipList $ f <$> a

-- discarding results in validation
data Person = Person 
  { age :: Int
  , name :: String
  }

validName :: Person -> Maybe String
validName person = 
  let name = validateName person <* validateAge person
   in
    -- ah yes, the famous double fmap
    fmap toUpper <$> name

-- I had these as Person -> Maybe Person before but that kind of doesn't
-- showcase dropping results; this way getting the <* wrong won't typecheck
validateName :: Person -> Maybe String
validateName (Person _ name) = 
  case name of
    "Matt" -> Just name
    _ -> Nothing

validateAge :: Person -> Maybe Int
validateAge (Person age _) =
  if age > 39
    then Just age
    else Nothing 

-- good commentary on "pure" - it says "actually this is a pure value, but it's
-- getting lifted into the functor" (although whomst among us hasn't pured a
-- side-effectful thing...)