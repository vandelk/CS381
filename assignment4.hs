-- Team Members: Kennedy Vandel and Michael Zavalza

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

rank :: Prog -> Rank -> Maybe Rank
rank (x:xs) r = undefined

rankP :: Prog -> Maybe Rank
rankP = undefined


sem :: Prog -> Stack -> Stack
sem p = undefined

--1b
semStatTC :: Prog -> Stack -> Stack
semStatTC p | rankP p >= 0 = sem p

-- The type of the new sem function would be Prog -> Stack -> Stack.
--It can be simplified to not use the Maybe data type because it would never be called in a situation that would be an error.

--2a
data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox = undefined

--2b
rect :: Shape -> Maybe BBox
rect = undefined

--3a
--1
--2
--3
--4

--3b


--3c


--3d
--Yes, you can. The result of the function would be another function of type a ->b
