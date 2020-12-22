module Party where

import Data.Tree
import Employee

-- 1

glCons :: Employee -> GuestList -> GuestList
glCons = undefined

-- 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node {rootLabel = x,subForest = []} = f x []
treeFold f Node {rootLabel = x,subForest = xs} =
  f x $ map (treeFold f) xs

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs = undefined

-- 3

nextLevel ::
  Employee ->
  [(GuestList, GuestList)] ->
  (GuestList, GuestList)
nextLevel = undefined

-- 4

maxFun :: Tree Employee -> GuestList
maxFun = undefined

-- 5

main :: IO ()
main = putStrLn "Hello World"
