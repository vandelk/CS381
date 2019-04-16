-- Group Members: Michael Zavalza and Kennedy Vandel
import HW1types
import Data.List hiding (intersect)

type Bag a = [(a,Int)]

type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
      | Circle Point Length
      | Rect Point Length Length
      deriving Show

type Figure = [Shape]
type BBox = (Point,Point)


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

--helper function for 1d
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


--just for testing
g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]
h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]


--2a
nodes :: Graph -> [Node]
nodes []          = []
nodes ((x,xs):ys) = norm ([x] ++ [xs] ++ nodes ys)


--2b
suc :: Node -> Graph -> [Node]
suc a [] = []
suc a ((x,xs):ys)   | a == x    = [xs] ++ suc a ys
                    | otherwise = suc a ys


--2c
detach :: Node -> Graph -> Graph
detach n [] = []
detach n ((x,xs):ys)  | x == n || xs == n = [] ++ detach n ys
                      | otherwise   = [(x,xs)] ++ detach n ys

--helper for 2d
cycloop :: Int -> Graph
cycloop 1 = []
cycloop x = cycloop (x-1) ++ [((x-1),x)]

--2d
cyc :: Int -> Graph
cyc x = cycloop x ++ [(x,1)]


-- for testing
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

--3a
width :: Shape -> Length
width (Pt (a,b))       = 0
width (Circle (a,b) y) = (2*y)
width (Rect (a,b) y z)  | y > z = y
                        | otherwise = z

--3b
bbox :: Shape -> BBox
bbox (Pt (a,b)) = ((a,b), (a,b))
bbox (Circle (a,b) y) = ((a-y,b-y), (a+y,b+y))
bbox (Rect (a,b) y z) = ((a,b), (a+y,b+z))

--3c
minX :: Shape -> Number
minX (Pt (a,b)) = a
minX (Circle (a,b) y) = (a-y)
minX (Rect (a,b) y z) = a


--3d

--helper function
addPt :: Point -> Point -> Point
addPt (a,b) (c,d) = (a+c, b+d)


move :: Shape -> Point -> Shape
-- This is what i was thinking but didn't work
-- move (Pt (a,b)) (c,d) = (Pt (addPt (a,b) (c,d)))
-- move (Circle (a,b) y) (c,d) = (Circle (addPt (a,b) (c,d)) y)
-- move (Rect (a,b) y z) (c,d) = (Rect (addPt (a,b) (c,d)) y z)

--3e
alignLeft :: Figure -> Figure
alignLeft = undefined


--3f
inside :: Shape -> Shape -> Bool
inside = undefined
