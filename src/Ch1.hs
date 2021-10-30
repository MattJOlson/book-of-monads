module Ch1 where

data Tree a 
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving stock (Show)

countLeaves :: Tree a -> Int
countLeaves = \case
    Leaf a -> 1
    Node tr tr' -> countLeaves tr + countLeaves tr'

labelLeaves :: Tree a -> Tree (Int, a)
labelLeaves tree = 
    fst $ labelSubtree tree 0
    where
        labelSubtree :: Tree a -> Int -> (Tree (Int, a), Int)
        labelSubtree tree start = 
            case tree of
                Leaf a -> (Leaf (start, a), start+1)
                Node l r -> labelNode l r start

        labelNode :: Tree a -> Tree a -> Int -> (Tree (Int, a), Int)
        labelNode l r start =
            let (l', next) = labelSubtree l start
                (r', stop) = labelSubtree r next
             in
                (Node l' r', stop) 

type WithCounter a = Int -> (a, Int)

-- ayy lmao
next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g = \i -> (let (r, j) = f i in g r j)

pure' :: a -> WithCounter a
pure' x = \i -> (x, i) -- warns, but the structure is instructive
-- pure' x i = (x, i) -- compiles both ways ofc

labelLeaves' :: Tree a -> Tree (Int, a)
labelLeaves' tree = fst $ go tree 0
    where go :: Tree a -> Int -> (Tree (Int, a), Int)
          go (Leaf a)   = \i -> (Leaf (i, a), i+1)
          go (Node l r) = go l `next` \l' ->
                          go r `next` \r' ->
                          pure' (Node l' r')

-- 1.1. Rewrite next, pure' in terms of State
type State' s a = s -> (a, s)

nextS :: State' s a -> (a -> State' s b) -> State' s b
f `nextS` g = \i -> (let (r, j) = f i in g r j)

pure'' :: a -> State' s a
pure'' x = \i -> (x, i)

-- 1.2. Write (++)
conc :: [a] -> [a] -> [a]
  -- lints to "foldr (:) r xs but that doesn't seem like the spirit of the q
conc [] r = r
conc (x:xs) r = x : conc xs r

-- 1.3. Write map
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) [] -- hah, gotcha hlint

-- 1.4. equivalent definitions of then_
-- I am not writing this out

-- notable comment:
-- "being a monad is not a property of a concrete type (like Int or Bool), but
--  of a type constructor (like Maybe or List)"