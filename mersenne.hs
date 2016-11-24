mersenne' 			:: Int -> Int -> Bool
mersenne' n m
  | 2 ^ n - 1 == m 	= True
  | 2 ^ n - 1 < m 	= mersenne' (n + 1) m
  | otherwise 		= False

mersenne 			:: Int -> Bool
mersenne 			= mersenne' 1
