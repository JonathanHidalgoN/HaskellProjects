{-
Program to do base conversion from numbers in base 2 to base 10 and vice-versa 
-}
import Data.Char

type Bit = Int

{-
Since a bin number is a sequence of 1's and 0's, we can put it as 
abc..., to convert to base 10, we can do the following.
(1 * a) + (2 * b) + (4 * c)
= a + 2 * (b + 2 * (c + 2 (d + 2 * 0)))
This is just a foldr with start value at 0 and apply the function
x + 2 * y
-}

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

{-
To convert a number to bin, we divide by 2 and take
the residue, then divide it by 2 until we got a 0.
-}
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

{-
Standarize so each number is 8 bits
-}
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

{-
Add 1 in the head if the number of ones
is even
-}
addParity :: [Bit] -> [Bit]
addParity xs 
    | even nos = 1 : xs
    | otherwise = 0 : xs
    where 
        nos =  length (filter (==1) xs)

{-
Conver a string to a sequence of bits
We have a string that is a list of characters, we apply a
map, so for each character we transform to ASCII with ord,
then it is an int, so we pass to binary, then make it 8 bits,
then concat each list
-}
encode :: String -> [Bit]
encode = addParity. concat . map (make8 . int2bin . ord)

{-
Given a list of bits, separate it into lists f 8 bits
-}
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

{-
Check parity in messages
-}
checkParity :: [Bit] -> [Bit]
checkParity xs 
    | even nos && head xs == 1 = tail xs
    | odd nos && head xs == 0 = tail xs 
    | otherwise = error "Corrputed"
    where 
        nos = length . filter (== 1) . tail $ xs

{-
Given a list of bits, we take each 8 bits(1 character),
pass to an int and to a character, then do it in the whole list of
bits
-}
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8 . checkParity

{-
Just the identity function
-}
channel :: [Bit] -> [Bit]
channel = id

{-
To check if it is working
-}
transmit :: String -> String
transmit = decode . channel . encode

