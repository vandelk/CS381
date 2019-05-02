-- Team Members: Kennedy Vandel and Michael Zavalza
import SVG

--1
type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP

type Stack = [Int]
type D = Maybe Stack -> Maybe Stack
type D1 = Stack -> Maybe Stack

semCmd :: Cmd -> D
semCmd (LD i)     (Just (x)) = Just ([i] ++ x)
semCmd ADD  (Just ((x:y:z))) = Just ([x+y] ++ z)
semCmd MULT (Just ((x:y:z))) = Just ([x*y] ++ z)
semCmd DUP   (Just ((x:y))) = Just ([x,x] ++ y)
semCmd _ _ = Nothing

sem1 :: Prog -> D
sem1 []     (Just (x)) = (Just (x))
sem1 (x:xs) (Just (i)) = sem1 xs (semCmd x (Just (i)))
sem1 _ _ = Nothing

sem :: Prog -> D1
sem x y = sem1 x (Just (y))


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
