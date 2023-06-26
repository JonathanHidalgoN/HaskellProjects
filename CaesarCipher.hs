{-
The Caesar cipher is an encryption algorithm that replaces each 
character in a string by shifting it a certain number of positions 
down the alphabet. 
This Haskell script provides a basic implementation to crack 
this encryption.
-}
import Data.Char

--Function that returns number of times an item appears in a list
count :: Eq a => a -> [a] -> Int
count item xs = 
    sum [1 | x <- xs, x == item]

-- Function that returns index where an element is 
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
    
countLetters :: String -> Int
countLetters xc = 
    sum [1 | x <- xc, isLetter x]
    
lower2int :: Char -> Int
lower2int c = 
    ord c - ord 'a' 

upper2int :: Char -> Int 
upper2int c = 
    ord c - ord 'A' 

-- Function that maps lower integers to characters
int2letL :: Int -> Char 
int2letL n = 
    chr(ord 'a' + n)
   
-- Function that maps upper integers to characters
int2letU :: Int -> Char 
int2letU n = 
    chr(ord 'A' + n)

-- Shift a letter n places 
shift :: Int -> Char -> Char
shift n c 
    | isLower c = int2letL((lower2int c + n ) `mod` 26)
    | isUpper c = int2letU((upper2int c + n) `mod` 26)
    | otherwise = c 

-- Apply shift function to each character in a string
encode :: Int -> String -> String
encode n xc =
    [shift n x | x <- xc]

-- Table with prob of letters 
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
    0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
    6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- Function that computes the % 
percent :: Int -> Int -> Float
percent n m = 
    (fromIntegral n / fromIntegral m) * 100

-- Function to calculate the frequency of a letter in a string
freqs :: String -> [Float]
freqs xc = 
    [
        percent (count x xc) n | x <- ['a'..'z']
    ]
    where n = countLetters xc    

-- Function that computes the chi-square sum for two list  
chisqr :: [Float] -> [Float] -> Float
chisqr os es =
     sum [((o-e)^2)/e | (o,e) <- zip os es]

-- Function that rotates a list n places
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- Composition of functions to crack a string
crack :: String -> String
crack xs =
    encode (-factor) xs
    where
        -- Take the factor with minimun chisqrt 
        factor = head (positions (minimum chitab) chitab)
        -- Compute chisqr for each possible table (26 letters)
        chitab = [chisqr (rotate n tab) table | n <- [0..25]]
        -- Compute freq table for xs
        tab = freqs xs


