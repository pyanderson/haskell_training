-- No modulo Data.List já existe a função intersect, porém, ela não
-- é importada pelo Prelude
find 			:: (Ord t) => t -> [t] -> Bool
find _ [] 		= False
find n (x:xs)
  | n == x 		= True
  | otherwise 	= find n xs

intersect 		:: (Ord t) => [t] -> [t] -> [t]
intersect xs ys = [x | x <- xs, find x ys]
