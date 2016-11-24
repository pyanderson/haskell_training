decToBin 			:: Int -> [Char]
decToBin 0			= ['0']
decToBin n
  | odd n			= decToBin(div n 2) ++ ['1']
  | otherwise			= decToBin(div n 2) ++ ['0']

charToInt 			:: Char -> Int
charToInt x 			= (read::[Char]->Int) [x]

binToDec' 			:: [Char] -> Int -> Int
binToDec' [] _ 			= 0
binToDec' (x:xs) n 		= (2 ^ n) * (charToInt x) + (binToDec' xs (n-1))

binToDec 			:: [Char] -> Int
binToDec xs 			= binToDec' xs ((length xs) - 1)
