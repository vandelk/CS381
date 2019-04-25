-- Team Members: Kennedy Vandel and Michael Zavalza

--1a

data Cmd = Pen Mode
         | Moveto (Pos, Pos)
         | Def String (Pars) Cmd
         | Call String (Vals)
         | AND Cmd Cmd

data Mode = Up | Down

data Pos = NUM Int | Name String

data Pars = MPars String Pars| Par String

data Vals = MVals Int Vals | Val Int


--1b
vector = Def "vector" (MPars "pos1"(Par "pos2")) (AND (Pen Down)(AND (Call "vector" (MVals 1(Val 2))) (AND (Moveto ((NUM 2),(NUM 2))) (Pen Up))))


--1c
steps :: Int -> Cmd
steps 0 = Pen Up
steps x = AND (steps (x-1)) (AND (Moveto (NUM (x-1), NUM(x))) (Moveto (NUM(x),NUM(x))))


--2a
data Circuit = Ct Gates Links

data Gates = Gt [(Int, GateFN)]

data GateFN = And | Or | Xor | Not

data Links = Lnk [ ((Int, Int),(Int, Int)) ]


--2b
c1 :: Circuit
c1 = Ct (Gt [(1, Xor),(2, And)]) (Lnk [((1,1),(2,1)), ((1,2),(2,2))])


--2c



-- 3 --
data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr

data Op = Add | Multiply | Negate
data Exp = Num Int
         | Apply Op [Exp]

--3a
x :: Exp
x = Apply Multiply [(Apply Negate [ (Apply Add [Num 3, Num 4]) ]) , Num 7]

--3b
--The alternative syntax has a distinction between an operation and an expression.
--The alternative syntax uses a list of expressions, which can be misleading,
--as this allows an input of 0 expressions or more than 2 expressions, neither of which are valid


--3c
translate :: Expr -> Exp
translate (N y) = Num y
translate (Plus y z) = Apply Add [translate y, translate z]
translate (Times y z) = Apply Multiply [translate y, translate z]
translate (Neg y) = Apply Negate [translate y]
