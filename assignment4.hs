-- Team Members: Kennedy Vandel and Michael Zavalza

type Prog = [Cmd]

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
rankC = undefined

rankP :: Prog -> Maybe Rank
rankP = undefined

--1b


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


--3b


--3c


--3d
