-- https://rosettacode.org/wiki/Primality_by_trial_division#Haskell
ePrimo				:: Int -> Bool
ePrimo n			= n==2 || n>2 && all ((> 0).rem n) (2:[3,5..floor.sqrt.fromIntegral $ n+1])

-- Versão mais detalhada, procura o primeiro divisor dentre os possíveis
-- divisores
ePrimo'Aux			:: Int -> [Int] -> Bool
ePrimo'Aux _ [] 		= True
ePrimo'Aux n (x:xs) 	| n `mod` x == 0 = False
  | otherwise = ePrimo'Aux n xs

ePrimo'				:: Int -> Bool
ePrimo' n			| n == 2 = True
  | n > 2 = ePrimo'Aux n (2:[3,5..floor.sqrt.fromIntegral $ n + 1])
  | otherwise = False
