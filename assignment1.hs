import HW1types
import Data.List hiding (intersect)
type Bag a = [(a,Int)]

--1a
ins :: Eq a => a -> Bag a -> Bag a
ins a []      = [(a, 1)]
ins a ((x,xs):ys)  | x == a = [(a, succ xs)] ++ ys
                   | otherwise   = [(x,xs)] ++ ins a ys


--1b
del :: Eq a => a -> Bag a -> Bag a
del a []      = []
del a ((x,xs):ys)  | x == a && xs == 1 = [] ++ ys
                   | x == a = [(a, (xs-1))] ++ ys
                   | otherwise   = [(x,xs)] ++ del a ys


--1c
bag :: Eq a => [a] -> Bag a
bag []     = []
bag (x:xs) = ins x (bag xs)


listsame :: Eq a => [a] -> [a] -> Bool
listsame [] [] = True
listsame x []  = False
listsame [] y  = False
listsame (x:xs) y = listsame xs (delete x y)


--1d
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag x y  = listsame (unbag x) (unbag (isbag x y))


--helper functions for 1e
unbag :: Eq a => Bag a -> [a]
unbag []          = []
unbag ((x,xs):ys) = [x] ++ unbag (del x ((x,xs):ys))


intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) ys    | x `elem` ys = [x] ++ intersect xs (delete x ys)
                       | otherwise   = intersect xs ys

--1e
isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag x y = bag (intersect (unbag x) (unbag y))


--1f
size :: Bag a -> Int
size []            = 0
size ((x,xs):ys)   = xs + size ys


--2a



--2b



--2c



--2d


--3a



--3b



--3c



--3d



--3e



--3f
