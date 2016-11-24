catIndices 		:: [Int] -> [t] -> [t]
catIndices xs ys 	= ys ++ [ys !! (i - 1) | i <- reverse(xs)]
