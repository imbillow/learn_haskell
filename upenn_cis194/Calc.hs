{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import qualified Data.Map as M
import ExprT
import Parser
import StackVM

-- 1

eval :: ExprT -> Integer
eval e =
  case e of
    ExprT.Lit i -> i
    ExprT.Add e1 e2 -> eval e1 + eval e2
    ExprT.Mul e1 e2 -> eval e1 * eval e2

-- 2

evalStr :: String -> Maybe Integer
evalStr str = do
  expr <- parseExp ExprT.Lit ExprT.Add ExprT.Mul str
  return $ eval expr

-- 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- 4

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = lit $ max a b
  mul (MinMax a) (MinMax b) = lit $ min a b

instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 a) (Mod7 b) = lit $ a + b
  mul (Mod7 a) (Mod7 b) = lit $ a * b

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

-- 5

instance Expr Program where
  lit i = [PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- 6

class HasVars a where
  var :: String -> a

data VarExprT
  = Lit' Integer
  | Add' VarExprT VarExprT
  | Mul' VarExprT VarExprT
  | Var' String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var'

instance Expr VarExprT where
  lit = Lit'
  add = Add'
  mul = Mul'

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = Just . const x
  add a b = \m -> a m >>= \x -> b m >>= \y -> return $ x + y
  mul a b = \m -> a m >>= \x -> b m >>= \y -> return $ x * y

withVars ::
  [(String, Integer)] ->
  (M.Map String Integer -> Maybe Integer) ->
  Maybe Integer
withVars vs exp = exp $ M.fromList vs
