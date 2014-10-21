
-- Assignment 4, CSCE-314
--
-- Student: Nicholas Warner
--

module Main where

import Prelude hiding (lookup)

import Test.HUnit
import System.Exit

-- AST definition for W
--VALUES
data WValue = VInt Int
            | VBool Bool deriving (Eq, Show)

--EXPRESSIONS
data WExp = Val WValue              --constant

          | Var String              --variable

          --Integer Expressions
          | Plus WExp WExp          --addition
          | Minus WExp WExp         --subtraction
          | Multiplies WExp WExp    --multiplication
          | Divides WExp WExp       --division

          --Boolean Expressions
          | Equals WExp WExp        --equality
          | NotEqual WExp WExp      --inversion
          | Less WExp WExp          --less than
          | Greater WExp WExp       --greater than
          | LessOrEqual WExp WExp   --less than or equal to
          | GreaterOrEqual WExp WExp--greater than or equal to

          --Logical Expressions
          | And WExp WExp           --logical and
          | Or WExp WExp            --logical or
          | Not WExp                --logical not

--STATEMENT
data WStmt = Empty
           | VarDecl String WExp  --Declare a Variable With an Initializer Expression
           | Assign String WExp   --Variable Assignment
           | If WExp WStmt WStmt  --if-statement
           | While WExp WStmt     --while-statement
           | Block [WStmt]        --a block (a list of statements considered as one)

type Memory = [(String, WValue)]

-- my helper functions for evaluations and executions
--Integer Expressions
mplus :: WValue -> WValue -> WValue
mplus (VInt a) (VInt b) = VInt ( a + b )
mplus _ _ = error "Incorrect Input on Addition\n"

mminus :: WValue -> WValue -> WValue
mminus (VInt a) (VInt b) = VInt ( a - b )
mminus _ _ = error "Incorrect Input on Subtraction\n"

mmultiply :: WValue -> WValue -> WValue
mmultiply (VInt a) (VInt b) = VInt ( a * b )
mmultiply _ _ = error "Incorrect Input on Multiplication\n"

mdivide :: WValue -> WValue -> WValue
mdivide (VInt a) (VInt b) = VInt ( a `div` b )
mdivide _ _ = error "Incorrect Input on Division\n"

--Boolean Expressions
mequals :: WValue -> WValue -> WValue
mequals (VInt a) (VInt b) = VBool ( a == b )
mequals _ _ = error "Incorrect Input on Equality\n"

mnotequal :: WValue -> WValue -> WValue
mnotequal (VInt a) (VInt b) = VBool ( a /= b )
mnotequal _ _ = error "Incorrect Input on Inverse Equality\n"

mless :: WValue -> WValue -> WValue
mless (VInt a) (VInt b) = VBool ( a < b )
mless _ _ = error "Incorrect Input on Less Than Operation\n"

mgreater :: WValue -> WValue -> WValue
mgreater (VInt a) (VInt b) = VBool ( a > b )
mgreater _ _ = error "Incorrect Input on Greater Than Operation\n"

mlessorequal :: WValue -> WValue -> WValue
mlessorequal (VInt a) (VInt b) = VBool ( a <= b )
mlessorequal _ _ = error "Incorrect Input on Equality and Less Than Operation\n"

mgreaterorequal :: WValue -> WValue -> WValue
mgreaterorequal (VInt a) (VInt b) = VBool ( a >= b )
mgreaterorequal _ _ = error "Incorrect Input on Equality and Greater Than Operation\n"

--Logical Expressions
mand :: WValue -> WValue -> WValue
mand (VBool a) (VBool b) = VBool ( a && b )
mand _ _ = error "Incorrect Input on AND operation\n"

mor :: WValue -> WValue -> WValue
mor (VBool a) (VBool b) = VBool ( a || b)
mor _ _ = error "Incorrect Input on OR Operation\n"

mnot :: WValue -> WValue
mnot (VBool a) = VBool ( not a )
mnot _ = error "Incorrect Input on NOT opertation\n"

-- eval function
eval :: WExp -> Memory -> WValue
eval ( Val x ) a = x
eval ( Var s ) xs = case lookup s xs of
                      Nothing -> error (s ++ "not Found")
                      Just v -> v
eval (Plus w1 w2) a = mplus (eval w1 a) (eval w2 a)
eval (Minus w1 w2) a =  mminus (eval w1 a) (eval w2 a)
eval (Multiplies w1 w2) a = mmultiply (eval w1 a) (eval w2 a)
eval (Divides w1 w2) a = mdivide (eval w1 a) (eval w2 a)
eval (Equals w1 w2) a = mequals (eval w1 a) (eval w2 a)
eval (NotEqual w1 w2) a = mnotequal (eval w1 a) (eval w2 a)
eval (Less w1 w2) a = mless (eval w1 a) (eval w2 a)
eval (Greater w1 w2) a = mgreater (eval w1 a) (eval w2 a)
eval (LessOrEqual w1 w2) a = mlessorequal (eval w1 a) (eval w2 a)
eval (GreaterOrEqual w1 w2) a = mgreaterorequal (eval w1 a) (eval w2 a)
eval (And w1 w2) a = mand (eval w1 a) (eval w2 a)
eval (Or w1 w2) a = mor (eval w1 a) (eval w2 a)
eval (Not w1) a = mnot (eval w1 a)

-- exec function
exec :: WStmt -> Memory -> Memory
exec Empty m = m
exec (VarDecl str w) a 
                    | (lookup str a) == Nothing = (str, (eval w a)) : a
                    | otherwise = error (str ++ "not Found")
exec (Assign str w) a = (str, (eval w a)) : a
exec (Block []) a = a
exec (Block (x:xs)) a = exec (Block xs) (exec x a)
{-exec (If w1 s1 _) a
                  | eval w1 a == VBool( True ) = exec s1 a
                  | otherwise = a-}       -- throws warning: Pattern match(es) are overlapped
                                          --                 In an equation for `exec': exec (If w1 s1 s2) a = ...
exec (If w1 s1 s2) a
                  | eval w1 a == VBool( True ) = exec s1 a
                  | eval w1 a == VBool( False ) = exec s2 a
                  
exec (While w1 s1) a = if ( eval w1 a == VBool( True ) ) then exec (While w1 s1) (exec s1 a) else a
                  
-- example programs
factorial = 
  Block
  [
    VarDecl "acc" (Val (VInt 1)),
    While (Greater (Var "x") (Val (VInt 1)))
    (
      Block
      [
        Assign "acc" (Multiplies (Var "acc") (Var "x")),
        Assign "x" (Minus (Var "x") (Val (VInt 1)))         
      ]
    )
  ]

p1 = Block
     [
       VarDecl "x" (Val (VInt 0)),
       VarDecl "b" (Greater (Var "x") (Val (VInt 0))),
       If (Or (Var "b") (Not (GreaterOrEqual (Var "x") (Val (VInt 0)))))
         ( Block [ Assign "x" (Val (VInt 1)) ] )
         ( Block [ Assign "x" (Val (VInt 2)) ] )
     ]

--addition
mp1 = Plus (Val(VInt 16)) (Val(VInt 116))

--subtraction
mp2 = Minus (Val(VInt 116)) (Val(VInt 16))

--multiplication
mp3 = Multiplies (Val(VInt 16)) (Val(VInt 116))

--division
mp4 = Divides (Val(VInt 256)) (Val(VInt 16))

--equal
mp5 = Block
      [
        VarDecl "a" (Val (VInt 0)),
        VarDecl "b" (Val (VInt 16)),
        If (Equals (Var "a") (Var "b"))
          ( Block [ Assign "a" (Val (VBool True)) ] )
          ( Block [ Assign "a" (Val (VBool False)) ] )
      ]

--not equals
mp6 = Block
      [
        VarDecl "c" (Val (VInt 0)),
        VarDecl "d" (Val (VInt 16)),
        If (NotEqual (Var "c") (Var "d"))
          ( Block [ Assign "c" (Val (VBool True)) ] )
          ( Block [ Assign "c" (Val (VBool False)) ] )
      ]

--greater than
mp7 = Block
      [
        VarDecl "e" (Val (VInt 0)),
        VarDecl "f" (Val( VInt 3)),
        While ( Greater (Var "f") (Var "e"))
        (
          Block [ Assign "e" (Plus (Var "e") (Val (VInt 1)) ) ]
        )
      ]

--greater than or equal to
mp8 = Block
      [
      ]

--less than
mp9 = Block
      [
      ]

--less than or equal to
mp10 = Block
      [
      ]


mp11 = Block
      [
      ]
mp12 = Block
      [
      ]
mp13 = Block
      [
      ]
mp14 = Block
      [
      ]
mp15 = Block
      [
      ]
mp16 = Block
      [
      ]
mp17 = Block
      [
      ]
mp18 = Block
      [
      ]
mp19 = Block
      [
      ]
mp20 = Block
      [
      ]

-- some useful helper functions
lookup s [] = Nothing
lookup s ((k,v):xs) | s == k = Just v
                    | otherwise = lookup s xs

asInt (VInt v) = v
asInt x = error $ "Expected a number, got " ++ show x

asBool (VBool v) = v
asBool x = error $ "Expected a boolean, got " ++ show x

fromJust (Just v) = v
fromJust Nothing = error "Expected a value in Maybe, but got Nothing"

-- unit tests
myTestList =

  TestList [
    test $ assertEqual "p1 test" 2 (asInt (fromJust (lookup "x" (exec p1 [])))),

    let res = lookup "acc" (exec factorial [("x", VInt 10)])
    in test $ assertBool "factorial of 10" (3628800 == asInt (fromJust res))
    ]    

-- main: run the unit tests  
main = do c <- runTestTT myTestList
          putStrLn $ show c
          let errs = errors c
              fails = failures c
          if (errs + fails /= 0) then exitFailure else return ()

