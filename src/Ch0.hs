module Ch0 where

import Data.List

-- | Chapter 0 exercises

-- 0.2 wants to define Eq for tuples, obviously there's already a tuple type
-- which has an instance of Eq so we're doing this instead
data Pair a b = Pair a b

instance (Eq a, Eq b) => Eq (Pair a b) where
    (==) (Pair a b) (Pair a' b') = 
        case (a == a', b == b') of
            (True, True) -> True
            _ -> False

-- skipping 0.3, 0.4 (scala)

-- 0.5: define Container on lists
class Container c where
    empty  :: c a
    insert :: a -> c a -> c a

instance Container [] where
    empty  = []
    insert x xs = x:xs