mult ::[Int] -> Int
mult lst = foldr (*) 1 lst
positive :: Int -> Bool
positive x = if x > 0 then True else False
postList :: [Int] -> [Int]
postList lst = filter positive lst
true' :: Bool -> Bool
true' x = if x == True then x else False
filterTrue :: [Bool] -> [Bool]
filterTrue lst = filter true' lst
trueList :: [Bool] -> Bool
trueList lst = if length lst == length (filterTrue lst) then True else False
judgeEven :: [Int] -> [Bool]
judgeEven lst = map (even) lst
evenList :: [Int] -> Bool
evenList lst = foldr (&&) True (judgeEven lst)
larger' :: Ord a => a -> a -> a
larger' y x = if y > x then y else x
maxList :: Ord a => [a] -> a
maxList lst = foldr (larger') (head lst) lst 
filterRange :: Int -> Int -> Int-> Bool
filterRange x y a = if (a >= x) && (a <= y) then True else False
inRange :: Int -> Int -> [Int] -> [Int]
inRange x y lst = filter (filterRange x y) lst
positive' :: [Int] -> [Int]
positive' lst = filter positive lst
countPositives :: [Int] -> Int
countPositives lst = length (positive' lst)
count' :: a -> Int
count' x = 1
countList :: [a] -> [Int]
countList lst = map (count') lst
myLength :: [a] -> Int
myLength lst = foldr (+) 0 (countList lst) 
mymap :: (a -> b) -> [a] -> [b]
mymap f xs = foldr (\y ys -> (f y):ys) [] xs
mylength' :: [a] -> Int 
mylength' ls = foldr (\_ y -> y+1) 0 ls
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f i [] = i
myfoldr f i lst = myfoldr f (f (last lst) i) (init lst)