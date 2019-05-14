-- Team Members: Kennedy Vandel and Michael Zavalza
import Data.Maybe
type Prog = [Cmd]
type Stack = [Int]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC
         | SWAP
         | POP Int

--1a
type Rank = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD _) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,3)
rankC DUP = (1,2)
rankC INC = (1,1)
rankC SWAP = (2,2)
rankC (POP x) = (x, 0)

validcmd :: CmdRank -> Rank -> Bool
validcmd (a,b) x | a > x = False
                 | otherwise = True

affectonrank :: CmdRank -> Int -> Int
affectonrank (a,b) x = (x - a + b)

rank :: Prog -> Rank -> Maybe Rank
rank [] r     = Just r
rank (x:xs) r | validcmd (rankC x) r = rank xs (affectonrank (rankC x) r )
              | otherwise = Nothing

rankP :: Prog -> Maybe Rank
rankP x = rank x 0



--1b
sem :: Prog -> Stack -> Stack
sem p = undefined

validexp :: Prog -> Bool
validexp p = isJust (rankP p)

semStatTC :: Prog -> Maybe (Stack -> Stack)
semStatTC p | validexp p = Just (sem p)
            | otherwise = Nothing

-- The type of the new sem function would be Prog -> Stack -> Stack.
--It can be simplified to not use the Maybe data type because it would never be called in a situation that would be an error.

--2a
data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox X                       = (1,1)
bbox (TD a b) | ax > bx      = (ax, ay + by)
              | ax <= bx     = (bx, ay + by)
              where (ax, ay) = bbox a
                    (bx, by) = bbox b
bbox (LR a b) | ay > by      = (ax + bx, ay)
              | ay <= by     = (ax + bx, by)
              where (ax, ay) = bbox a
                    (bx, by) = bbox b

--2b
rect :: Shape -> Maybe BBox
rect X        = Just (1,1)
rect (TD a b) = case rect a of --first shape
                     Nothing -> Nothing
                     Just (ax, ay) -> case rect b of --second shape
                                      Nothing -> Nothing
                                      Just (bx, by) -> case (ax == bx) of
                                                       True -> Just (ax, ay + by)
                                                       False -> Nothing
rect (LR a b) = case rect a of --first shape
                     Nothing -> Nothing
                     Just (ax, ay) -> case rect b of --second shape
                                      Nothing -> Nothing
                                      Just (bx, by) -> case (ay == by) of
                                                       True -> Just (ax + bx, ay)
                                                       False -> Nothing

--3a
--1 The type of f is a list of the type of y. The type of g is just a list.
--2 In a statically typed language, all of the return types must match.
--3 g is more general
--4 g has a different type from f because of its second line.
--In that case, it can return an empty list, without any specific data type attached to it.

--3b
h (x:xs) ((z,w):ys) = (w:xs)

--3c


--3d
--No, you can't. It would extremely difficult to come up with a function that takes a value of some arbitrary type a and returns a value of some other arbitrary type b.
