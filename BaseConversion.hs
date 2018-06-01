import Data.Char (ord, chr)
import Data.List
-- Base conversion
-- example >bin2int[1,0,1,1] = 13
type Bit = Int

bin2int :: [Bit] -> Int
--bin2int bits = sum [ w * b | (w,b) <- zip weights bits ]
-- where weights = iterate (*2) 1
 
bin2int = foldr (\x y -> x + 2 * y) 0

---------------------------------------------------------------------------
-- example >int2bin 13 = [1,0,1,1] 
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

---------------------------------------------------------------------------
-- example >make8[1,0,1,1] = [1,0,1,1,0,0,0,0]

make8 :: [Bit] -> [Bit]
make8 bits = take 8(bits ++ repeat 0)

---------------------------------------------------------------------------
-- Transmission
-- example >encode "abc" = [1,0,0,0,0,1,1,0,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- example > chop8 [1,0,0,0,0,1,1,0,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] 
--                 = [[1,0,0,0,0,1,1,0],[0,0,1,0,0,0,1,1],[0,1,1,0,0,0,1,1],[0]]

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8(drop 8 bits)

-- example > decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] = "abc"
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- example > transmit "higher-order functions are easy" 
--           = "higher-order functions are easy"

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

---------------------------------------------------------------------------
-- Voting Algorithms --

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- example > count "Red" votes = 2
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x) 

-- example > rmdups votes = ["Red", "Blue", "Green"]

rmdups :: Eq a => [a] -> [a]
rmdups [] =   []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- example > result votes = [(1,"Green"), (2,"Red"), (3,"Blue")]

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

-- example > winner votes = "Blue"

winner :: Ord a => [a] -> a
winner = snd .last . result

---------------------------------------------------------------------------
-- Alternative Vote --

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

-- example > rmempty [["Red","Green"],[],["Green","Red"],["Green","Red"],["Green"]]
--          = [["Red","Green"],["Green","Red"],["Green","Red"],["Green"]]
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- example > elim "Blue" ballots = [["Red","Green"],[],["Green","Red"],["Green","Red"],["Green"]]
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

-- example > rank ballots = ["Red", "Blue", "Green"]

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- example > winner’ ballots = "Green"

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
 [c]-> c
 (c:cs) -> winner' (elim c bs)

 --------------------------------------------------------------------------
 --7.9Exercises

-- 1.Show how the list comprehension [f x | x <- xs, p x] can be reexpressed
-- using the higher-order functions map and filter.
-- map f (filter p xs)
-- example: elistComprehension [1,2,3,4,5] = [8,10]

elistComprehension lista = map(*2) (filter (>3) lista)

---------------------------------------------------------------------------
-- 2.Without looking at the definitions from the standard prelude, define the
-- following higher-order library functions on lists.

-- a.Decide if all elements of a list satisfy a predicate:
--    all :: (a -> Bool) -> [Bool] -> Bool
-- example: eAll (>2) [1,2,3] = False ; eAll (>2) [3,4,5] = True

eAll :: (a -> Bool) -> [a] -> Bool
eAll f [] = True
eAll f (x:xs) = f x && eAll f xs

---------------------------------------------------------------------------
-- b.Decide if any element of a list satisfies a predicate:
--    any :: (a -> Bool) -> [Bool] -> Bool
-- example: eAny (>2) [1,2,3] = True ; eAny (>2) [1,2,2] = False

eAny :: (a -> Bool) -> [a] -> Bool
eAny f [] = False
eAny f (x:xs) = f x || eAny f xs

---------------------------------------------------------------------------
-- c.Select elements from a list while they satisfy a predicate:
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- example: eTakeWhile (>2) [4,6,4,1,2,3] = [4,6,4]

eTakeWhile :: (a -> Bool) -> [a] -> [a]
eTakeWhile _ [] = []
eTakeWhile f (x:xs) | f x = x : eTakeWhile f xs
                             |otherwise = []

---------------------------------------------------------------------------
-- d.Remove elements from a list while they satisfy a predicate:
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- example: eDropWhile (>2) [4,6,4,1,2,3] = [1,2,3]

eDropWhile :: (a -> Bool) -> [a] -> [a]
eDropWhile _ [] = []
eDropWhile f (x:xs) | f x  = eDropWhile f xs
                             | otherwise = x : xs

---------------------------------------------------------------------------
-- 3.Redefine the functions map f and filter p using foldr.
-- example:  eMap (>2) [1,2,3] = [False,False,True]

eMap :: (a -> b) -> [a] -> [b]
eMap f = foldr (\x xs -> f x : xs) []

-- example:  eFilter (==2) [1,2,3] = [2]

eFilter :: (a -> Bool) -> [a] -> [a]
eFilter p = foldr (\x xs -> if p x then (x : xs) else (xs)) []

---------------------------------------------------------------------------
-- 4.Using foldl, define a function dec2int :: [Int] -> Int that
-- converts a decimal number into an integer. For example:
-- example > dec2int [2,3,4,5] = 2345

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

---------------------------------------------------------------------------
-- 5.Without looking at the definitions from the standard prelude, define the
-- higher-order library function curry that converts a function on pairs
-- into a curried function, and, conversely, the function uncurry that
-- converts a curried function with two arguments into a function on pairs.
-- Hint: first write down the types of the two functions.
-- example: eCurry snd 5 3 = 3

eCurry :: ((a,b) -> c) -> a -> b -> c
eCurry f = \x y -> f (x, y)

-- example: eUncurry (+) (1,2) = 3

eUncurry :: (a -> b -> c) -> ((a, b) -> c)
eUncurry f = \(x, y) -> f x y

---------------------------------------------------------------------------
-- 6.A higher-order function unfold that encapsulates a simple pattern of
--    recursion for producing a list can be defined as follows:

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
    | p x       = []
    | otherwise = (h x):(unfold p h t (t x))

-- That is, the function unfold p h t produces the empty list if the
-- predicate p is true of the argument value, and otherwise produces a
-- non-empty list by applying the function h to this value to give the
-- head, and the function t to generate another argument that is
-- recursively processed in the same way to produce the tail of the list.
-- For example, the function int2bin can be rewritten more compactly
-- using unfold as follows:

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- Redefine the functions chop8, map f and iterate f using unfold.

-- example > eChop8 [1,0,0,0,0,1,1,0,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] 
--                 = [[1,0,0,0,0,1,1,0],[0,0,1,0,0,0,1,1],[0,1,1,0,0,0,1,1],[0]]

eChop8 :: [Int] -> [[Int]]
eChop8 = unfold (null) (take 8) (drop 8)

-- example: eMap' (>2) [1,2,3] = [False,False,True]

eMap' :: (a -> b) -> [a] -> [b]
eMap' f = unfold null (f.head) tail

-- example: take 10 (eIterate (+2) 10) = [10,12,14,16,18,20,22,24,26,28]

eIterate :: (a -> a) -> a -> [a]
eIterate f = unfold (const False) id f

-- example: eMap'' (+10) (+100) [0,1,2,3,4] = [10,12,14,101,103]

eMap'' :: (Foldable t1, Integral a) => (t2 -> a) -> (t2 -> a) -> t1 t2 -> [a]
eMap'' f1 f2 = foldr (\x xs -> filter even ( (f1)  x : xs) ++ filter odd ( (f2)  x : xs) ) []

-- Ejercicios sacados del libro: Programming in Haskell - Graham Hutton 2da edición [2017]