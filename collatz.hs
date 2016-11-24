collatz				:: Int -> [Int]
collatz 1			= [1]
collatz n
  | odd n			= n:(collatz ((n * 3) + 1))
  | otherwise			= n:(collatz (div n 2))

tamCollatz			:: Int -> Int
tamCollatz			= length . collatz

allCollatz			:: [Int] -> [[Int]]
allCollatz xs			= [collatz x | x <- xs]

allTamCollatz			:: [Int] -> [(Int, Int)]
allTamCollatz xs		= [(x, tamCollatz x) | x <- xs]
