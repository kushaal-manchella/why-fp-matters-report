--CHAPTER 3: GLUING FUNCTIONS TOGETHER.  (using higher order functions)
--How higher order functions can be re-used to solve multiple different problems

-- a. list operations using reduce
-- defining a list
data ListOf x = Nil                
              | Cons x (ListOf x)  
              deriving (Show, Eq)

-- example of list
exampleList :: ListOf Int
exampleList = Cons 1 (Cons 2 (Cons 3 Nil))


-- custom reduce for ListOf
reduce:: (t1 -> t2 -> t2) -> t2 -> ListOf t1 -> t2
reduce _ acc Nil = acc
reduce f acc (Cons x xs) = f x (reduce f acc xs)

-- custom function to sum up a list 
sumList :: ListOf Int -> Int
sumList = reduce (+) 0


-- example usage with sum of exampleList
-- >sumList exampleList 
-- 6 


-- custom function to find product of list
productList :: ListOf Int -> Int
productList = reduce (*) 1

-- example usage with product of exampleList 
-- >productList exampleList
-- 6


-- custom function to test if any element in a list is True
anytrue :: ListOf Bool -> Bool
anytrue = reduce (||) False 


-- custom function to test all elements in a list are True 
alltrue :: ListOf Bool -> Bool
alltrue = reduce (&&) True


-- boolList
boolList :: ListOf Bool
boolList = Cons False (Cons True Nil)


-- example usage with anytrue
-- >anytrue boolList
-- True

-- example usage with alltrue
-- >alltrue boolLIst
-- False


-- Append function using reduce
append :: ListOf a -> ListOf a -> ListOf a
append a b = reduce Cons b a

-- Example lists
list1 :: ListOf Int
list1 = Cons 1 (Cons 2 Nil)

list2 :: ListOf Int
list2 = Cons 3 (Cons 4 Nil)

-- Example usage with append
-- >append list1 list2
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

-- Double function
double :: Int -> Int
double n = 2 * n

-- Doubleandcons function using fandcons
fandcons :: (a -> b) -> a -> ListOf b -> ListOf b
fandcons f el list = Cons (f el) list

-- Cons as function composition
fandcons' :: (a -> b) -> a -> ListOf b -> ListOf b
fandcons' f = Cons . f

-- Doubleandcons using fandcons
doubleandcons' :: Int -> ListOf Int -> ListOf Int
doubleandcons' = fandcons double

-- Doubleall function using reduce
doubleall :: ListOf Int -> ListOf Int
doubleall = reduce (Cons . double) Nil

-- Example usage with doubleall
-- >doubleall intList
-- Cons 2 (Cons 4 (Cons 6 Nil))

-- Map function using reduce
customMap :: (a -> b) -> ListOf a -> ListOf b
customMap f = reduce (Cons . f) Nil

-- Doubleall using map
doubleall' :: ListOf Int -> ListOf Int
doubleall' = customMap double

-- Example usage with doubleall'
-- >doubleall' intList
-- Cons 2 (Cons 4 (Cons 6 Nil))



--b. tree operations using reduce
-- Define the custom TreeOf type
data TreeOf a = Node a (ListOf (TreeOf a)) deriving (Show, Eq)

-- Custom redtree function for TreeOf
redtree :: (a -> b -> b) -> (b -> b -> b) -> b -> TreeOf a -> b
redtree f g a (Node label subtrees) = f label (redtree' f g a subtrees)

redtree' :: (a -> b -> b) -> (b -> b -> b) -> b -> ListOf (TreeOf a) -> b
redtree' _ _ a Nil = a
redtree' f g a (Cons subtree rest) = g (redtree f g a subtree) (redtree' f g a rest)

-- Sumtree function to sum all labels in a tree of numbers
sumTree :: Num a => TreeOf a -> a
sumTree = redtree (+) (+) 0

-- Labels function to list all labels in a tree
labels :: TreeOf a -> ListOf a
labels = redtree Cons append Nil

-- Append function using reduce
appendTree :: ListOf a -> ListOf a -> ListOf a
appendTree a b = reduce Cons b a

-- Maptree function to apply a function to all labels in a tree

-- mapTree :: (a -> b) -> TreeOf a -> TreeOf b
-- mapTree f (Node label subtrees) = Node (f label) (mapSubtrees subtrees)
--   where
--     mapSubtrees :: ListOf (TreeOf a) -> ListOf (TreeOf b)
--     mapSubtrees Nil = Nil
--     mapSubtrees (Cons t ts) = Cons (mapTree f t) (mapSubtrees ts)

-- Example tree
exampleTree :: TreeOf Int
exampleTree = Node 1 (Cons (Node 2 Nil) (Cons (Node 3 (Cons (Node 4 Nil) Nil)) Nil))


--CHAPTER 4: GLUING PROGRAMS TOGETHER (using lazy evaluation)

-- a. Approximating Square root
-- Define the next function
next :: Double -> Double -> Double
next n x = (x + n / x) / 2

-- Define the repeat function
myRepeat :: (a -> a) -> a -> [a]
myRepeat f a = a : myRepeat f (f a)

-- Define the within function
within :: Double -> [Double] -> Double
within eps (a:b:rest)
  | abs (a - b) <= eps = b
  | otherwise          = within eps (b : rest)
within _ _ = error "within called with a list that is too short"

-- Define the sqrt function using within
sqrtNR :: Double -> Double -> Double -> Double
sqrtNR a0 eps n = within eps (myRepeat (next n) a0)

-- Define the relative function
relative :: Double -> [Double] -> Double
relative eps (a:b:rest)
  | abs (a - b) <= eps * abs b = b
  | otherwise                  = relative eps (b : rest)
relative _ _ = error "relative called with a list that is too short"

-- Define the relativesqrt function using relative
relativeSqrt :: Double -> Double -> Double -> Double
relativeSqrt a0 eps n = relative eps (myRepeat (next n) a0)

-- Example usage of relativeSqrt: 
-- >relativeSqrt 1.0 1e-10 25.0
-- 5.0


--b. Approximating gradient using differentiation and step sizes 

-- Basic numerical differentiation function
easydiff :: (Double -> Double) -> Double -> Double -> Double
easydiff f x h = (f (x + h) - f x) / h

-- Function to halve the value
halve :: Double -> Double
halve x = x / 2

-- Infinite sequence generation function
repeatFunction :: (a -> a) -> a -> [a]
repeatFunction f a = a : repeatFunction f (f a)

-- Generate sequence of approximations
differentiate :: Double -> (Double -> Double) -> Double -> [Double]
differentiate h0 f x = map (easydiff f x) (repeatFunction halve h0)


--b. integration example 
-- Basic integration function
easyintegrate :: (Double -> Double) -> Double -> Double -> Double
easyintegrate f a b = (f a + f b) * (b - a) / 2

-- Generate a sequence of better approximations
integrate :: (Double -> Double) -> Double -> Double -> [Double]
integrate f a b = integ f a b (f a) (f b)

integ :: (Double -> Double) -> Double -> Double -> Double -> Double -> [Double]
integ f a b fa fb = 
    let m = (a + b) / 2
        fm = f m
    in ((fa + fb) * (b - a) / 2) : zipWith (+) (integ f a m fa fm) (integ f m b fm fb)




main :: IO ()
main = do
-- sumList example
    let sumResult = sumList exampleList
    putStrLn $ "Sum of exampleList: " ++ show sumResult

    -- productList example
    let productResult = productList exampleList
    putStrLn $ "Product of exampleList: " ++ show productResult

    -- anytrue and alltrue examples
    let anyTrueResult = anytrue boolList
    putStrLn $ "Any true in boolList: " ++ show anyTrueResult

    let allTrueResult = alltrue boolList
    putStrLn $ "All true in boolList: " ++ show allTrueResult

    -- append example
    let appendedList = append list1 list2
    putStrLn $ "Appended list: " ++ show appendedList

    -- doubleall example
    let doubledList = doubleall exampleList
    putStrLn $ "Doubled list using reduce: " ++ show doubledList

    -- doubleall' example
    let doubledList' = doubleall' exampleList
    putStrLn $ "Doubled list using customMap: " ++ show doubledList'

    -- sumTree example
    let treeSum = sumTree exampleTree
    putStrLn $ "Sum of all labels in exampleTree: " ++ show treeSum

    -- labels example
    let treeLabels = labels exampleTree
    putStrLn $ "Labels in exampleTree: " ++ show treeLabels

    -- sqrtNR example
    let sqrtResult = sqrtNR 1.0 1e-10 25.0
    putStrLn $ "Square root of 25.0 using Newton-Raphson: " ++ show sqrtResult

    -- relativeSqrt example
    let relativeSqrtResult = relativeSqrt 1.0 1e-10 25.0
    putStrLn $ "Relative square root of 25.0: " ++ show relativeSqrtResult

    -- differentiation example
    let f x = x^2  -- Example function
    let x = 2.0    -- Point at which to differentiate
    let h0 = 1.0   -- Initial value of h
    let eps = 1e-10
    let derivative = within eps (differentiate h0 f x)
    putStrLn $ "The derivative of f at " ++ show x ++ " is approximately " ++ show derivative
    
    -- integration example
    let f x = x^2  -- Example function
    let a = 0.0    -- Lower bound
    let b = 1.0    -- Upper bound
    let eps = 1e-10
    let integral = within eps (integrate f a b)
    putStrLn $ "The integral of f from " ++ show a ++ " to " ++ show b ++ " is approximately " ++ show integral












