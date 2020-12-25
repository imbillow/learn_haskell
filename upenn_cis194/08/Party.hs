module Party where

import Data.Tree
import Data.List
import Employee

-- 1

glCons :: Employee -> GuestList -> GuestList
glCons x@Emp {empFun = f} (GL xs fs) = GL (x : xs) $ f + fs

instance Semigroup GuestList where
  GL xs f0 <> GL ys f1 = GL (xs ++ ys) $ f0 + f1

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node {rootLabel = x, subForest = xs} =
  f x $ map (treeFold f) xs

getFun :: GuestList -> Fun
getFun (GL _ f) = f

getEmps :: GuestList -> [Employee]
getEmps (GL emps _) = emps

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs x@Emp {empFun = f} [] = GL [x] f
combineGLs e xs
  | fe >= fxs = e'
  | otherwise = xs'
  where
    fe = empFun e
    e' = GL [e] fe
    fxs = sum $ map getFun xs
    xs' = GL (getEmps $ mconcat xs) fxs

-- 3

nextLevel ::
  Employee ->
  [(GuestList, GuestList)] ->
  (GuestList, GuestList)
nextLevel e@Emp {empFun = f} s = (GL (e : ys') (f + fy), mconcat xs)
  where
    (xs, ys) = unzip s
    GL ys' fy = mconcat ys

-- 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- 5

main :: IO ()
main = do
  text <- readFile "company.txt"
  let GL es f = maxFun (read text :: Tree Employee)
  putStrLn $ "Total fun: " ++ show f
  putStrLn $ unlines $ sort $ map empName es
