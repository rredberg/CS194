{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where
import Parser
import Data.Maybe
import qualified Data.Map as M


reify :: VarExprT -> VarExprT
reify = id

-- Exercise 3
class Expr a where
      add :: a -> a -> a
      mul :: a -> a -> a
      lit :: Integer -> a

instance Expr Integer where
    add x y = x + y
    mul x y = x * y
    lit x = x

instance Expr Bool where
    add x y = x || y
    mul x y = x && y
    lit x
       | x <= 0 = False
       | otherwise = True

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)
    lit x = MinMax x

instance Expr Mod7 where
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x+y) 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x*y) 7)
    lit x = Mod7 x

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
    var str = Var str

instance Expr VarExprT where
    add x y = Add x y
    mul x y = Mul x y
    lit x = Lit x

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var str = M.lookup str

instance Expr (M.Map String Integer -> Maybe Integer) where
    add x y e
         | x e == Nothing = Nothing
         | y e == Nothing = Nothing
         | otherwise = let (Just a) = x e
                           (Just b) = y e
                            in Just (a + b)
    --add x y = \e -> let (Just a) = x e
    --                    (Just b) = y e
    --                    in Just (a + b)
    -- This ^ one mostly works but errors if it can't look up a variable in the mapping
    --add x y = let (Just a) = e x
    --              (Just b) = e x
    --           in \e -> Just (a + b)
    --add x y = \e -> Just (a + b)
    --   where (Just a) = e x
    --         (Just b) = e y
    mul x y = \e -> let (Just a) = x e
                        (Just b) = y e
                        in Just (a * b)
    --lit = (\x y -> Just x)
    lit x y = Just x
    --lit x= (\y -> Just x) think this works too!
    --and maybe also: lit x y = Just x


withVars :: [(String, Integer)]
 -> (M.Map String Integer -> Maybe Integer)
 -> Maybe Integer
withVars vs exp = exp $ M.fromList vs