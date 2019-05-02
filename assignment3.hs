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
type Prog2 = [Cmd2]

data Cmd2 = LD2 Int
          | ADD2
          | MULT2
          | DUP2
          | DEF String [Cmd2]
          | CALL String

--2b
type Macros = [(String,Prog)]
type State = (Stack,Macros)

--2c
sem2 :: Prog2 -> State -> State
sem2 [] x = x
sem2 (x:xs) i = sem2 xs (semCmd2 x i)

semCmd2 :: Cmd2 -> State -> State
semCmd2 (LD2 i)     (x,m) = (x ++ [i],m)
-- semCmd2 ADD2        (x,m) =
-- semCmd2 MULT2       (x,m) =
-- semCmd2 DUP2        (x,m) =
-- semCmd2 (DEF str c) (x,m) =
--semCmd2 (CALL str)  (x,m) =

--3
data CMD = Pen Mode
         | MoveTo Int Int
         | Seq CMD CMD

data Mode = Up | Down

type STATE = (Mode,Int,Int)

semS :: CMD -> STATE -> (STATE,Lines)
semS = undefined
-- semS (Seq x y) s = (fst(semS y (fst semS x s))) , ((snd(semS x s)) ++ (snd(semS y (fst(semS x s)))))
-- semS (Pen Up) (Up,x,y) = ((Up,x,y), [])
-- semS (Pen Up) (Down,x,y) = ((Up,x,y), [])
-- semS (Pen Down) (Up,x,y) = ((Down,x,y), [])
-- semS (Pen Down) (Down,x,y) = ((Down,x,y), [])
-- semS (MoveTo x y) (Up,w,z) = ((Up,x,y),[])
-- semS (MoveTo x y) (Down,w,z) = ((Down,x,y),[x,y,w,z])


sem' :: CMD -> Lines
sem' x = snd(semS x (Up,0,0))
