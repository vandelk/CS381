-- Team Members: Kennedy Vandel and Michael Zavalza

--1a

data Cmd = Pen Mode
         | Moveto (Pos, Pos)
         | Def String (Pars) Cmd
         | Call String (Vals)
         | Cmd Cmd

data Mode = Up | Down

data Pos = NUM Int | Name String

data Pars = MPars String Pars| Par String

data Vals = MVals Int Vals | Val Int


--1b --add this shit



--1c --add this shit


steps :: Int -> Cmd
steps x = undefined

--2a
data Circuit = Ct Gates Links

data Gates = Gt [(Int, GateFN)]

data GateFN = And | Or | Xor | Not

data Links = Lnk [ ((Int, Int),(Int, Int)) ] --add this shit


--2b --add this shit



--2c --add this shit




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
translate = undefined
