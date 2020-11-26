import Data.Char
inRange :: Int -> Int -> [Int] -> [Int] 
and' :: Bool -> Bool -> Bool
True `and'` True = True
False `and'` False = False
True `and'` False = False
False `and'` True = False
inRange x y [] = []
inRange x y (z:zs) = (if (z>=x) `and'` (z<=y) then [z] else []) ++ inRange x y zs 
countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs) = (if x>0 then 1 else 0) + countPositives xs
capitalised :: String -> String
capitalised "" = ""
capitalised (x:[]) = [toUpper x]
capitalised (x:xs) = capitalised [x] ++ [toLower x| x<-xs]
capitalised1 :: String -> String
capitalised1 [] = []
capitalised1 [a] = [toUpper a]
capitalised1 (x:rest) = [toUpper x] ++ [toLower (head rest)] ++ (tail(capitalised rest))
title :: [String] -> [String] 
title ([]) = []
title (x:xs) = [(if length x > 4 then capitalised x else x)] ++ title xs
insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs) = if x<y then x:insert y xs else y:x:xs
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:[]) = [x]
isort (x:tail) = insert x (isort tail)
merge :: Ord a => [a] -> [a] -> [a] 
merge [] [] = []
merge x [] = x
merge [] x = x
merge (x:[]) (y:[]) = if x<y then [x,y] else [y,x]
merge (x:xs) (y:ys) = (if x<=y then [x] ++ merge xs (y:ys) else [y]++ merge (x:xs) ys)
firstFew :: Ord a => [a] -> [a]
firstFew [] = []
firstFew lst = take ((length lst) `div` 2) lst
lastFew :: Ord a => [a] -> [a]
lastFew [] = []
lastFew lst = drop ((length lst) `div` 2) lst
msort :: Ord a => [a] -> [a] 
msort [] = []
msort lst = if length lst == 1 then lst 
            else merge (msort (firstFew lst)) (msort (lastFew lst))
-- simple cipher
rotor :: Int -> String -> String
or' :: Bool -> Bool -> Bool
True `or'` True = True
False `or'` False = False
True `or'` False = True
False `or'` True = True
rotor x str = if (x < 0) `or'` (x >= length str) then error "invalid shift"
              else drop (x `mod`26) str ++ take (x`mod`26) str
makeKey :: Int -> [(Char,Char)] 
makeKey x =  zip ['A'..'Z'] (rotor x ['A'..'Z'])
lookUp :: Char -> [(Char,Char)] -> Char
lookUp x [] = x
lookUp x (k:keys) = if fst k == x then snd k else lookUp x keys
encipher :: Int -> Char -> Char
encipher x c = lookUp c (makeKey x)
normalise :: String -> String
normalise [] = []
normalise (x:xs) = (if x `elem` ['A'..'Z'] then [x] 
                   else if x `elem` ['a'..'z'] then [toUpper x] 
                   else []) ++ normalise xs
encipherStr :: Int -> String -> String
encipherStr n [] = []
encipherStr n (x:xs) = [encipher n x] ++ encipherStr n xs
-- most common character is E
count :: String -> Char-> Int
count [] c = 0
count (x:xs) c = (if x == c then 1 else 0) + count xs c
frequency :: String -> [(Char,Int)]
frequency [] = []
frequency (x:xs) = [(c,(count (x:xs) c))|c <- ['A'..'Z']]
lookUpchar :: Int -> [(Char,Int)] -> Char
lookUpchar x [] = '§'
lookUpchar x (k:keys) = if snd k == x then fst k else lookUpchar x keys
mostCommon :: String -> Char
mostCommon [] = '§'
mostCommon str = lookUpchar (maximum[b|(a,b)<-frequency str]) (frequency str)
lookUpint :: Char -> [(Char,Int)] -> Int
lookUpint x [] = 0
lookUpint x (k:keys) = if fst k == x then snd k else lookUpint x keys
shift :: String -> Int
helper = zip ['A'..'Z'] [0..25]
shift "" = 0
shift (x:xs) = 26 - ( (lookUpint (mostCommon (x:xs)) helper) - (lookUpint 'E' helper))
tonum str = [b|(a,b)<-helper, c <- str , c == a]
decode str = [a|(a,b)<-helper, i <- tonum str, ((shift str) + i) `mod`26 == b]