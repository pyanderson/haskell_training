ePerfeito 			:: Int -> Bool
ePerfeito n     	= n == sum [x | x <- [1..(div n 2)], n `mod` x == 0]

-- Verificar quais os numeros perfeitos de uma lista
saoPerfeitos 		:: [Int] -> [Int]
saoPerfeitos xs 	= [x | x <- xs, ePerfeito x]

-- Verificar quais os números perfeitos menor que n
perfeitosAteN 		:: Int -> [Int]
perfeitosAteN n 	= saoPerfeitos [1..n]

-- Quantidade de números perfeitos menores que n
quantPerfeitoAteN 	:: Int -> Int
quantPerfeitoAteN 	= length . perfeitosAteN
