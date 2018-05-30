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
--                 = [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]

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

 
 
 -- Ejercicios sacados del libro: Programming in Haskell - Graham Hutton 2da edición [2017]