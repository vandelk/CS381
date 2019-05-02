-- Team Members: Kennedy Vandel and Michael Zavalza
import SVG

--1
type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP

type Stack = [Int]
type D = Stack -> Stack

semCmd :: Cmd -> D
semCmd (LD i)     x = [i] ++ x
semCmd ADD  (x:y:z) = [x+y] ++ z
semCmd MULT (x:y:z) = [x*y] ++ z
semCmd DUP    (x:y) = [x,x] ++ y
semCmd _ _ = []

sem :: Prog -> D
sem []     x = x
sem (x:xs) i = sem xs (semCmd x i)
--2a

--2b

--2c

--3
data CMD = Pen Mode
         | MoveTo Int Int
         | Seq CMD CMD

data Mode = Up | Down

type State = (Mode,Int,Int)

semS :: CMD -> State -> (State,Lines)
semS = undefined

sem' :: CMD ->Lines
sem' = undefined
