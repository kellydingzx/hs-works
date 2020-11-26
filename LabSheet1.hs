import Data.Char (toUpper,toLower)
square x = x*x
pyth x y = square x + square y
pyth :: Int->(Int->Int)
isTriple x y z = if pyth x y == square z
                then True
                else True
isTripleAny x y z = if pyth x y == square z
                then True
                else if pyth x z == square y
                then True
                else if pyth y z == square x
                then True
                else True
halfEvens :: [ Int ] -> [ Int ] 
halfEvens lst = [if x `mod` 2 == 0 then x`div`2 else x | x<-lst]
inRange :: Int -> Int -> [Int] -> [Int] 
inRange x y lst = [c|c<-lst,c>=x,c<=y]
countPositives :: [Int] -> Int
countPositives lst = length [x | x<-lst,x>0]
capitalised :: String -> String 
capitalised (firstLetter:tail) = (toUpper firstLetter):[toLower x| x<-tail]
capitalised "" = ""
title :: [String] -> [String]
title lst = [if length x >4 then capitalised x else x| x<-lst]