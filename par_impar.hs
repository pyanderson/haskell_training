separa 			:: [t] -> [[t]]
separa xs		= [[xs !! i | i <- [0..(length xs) - 1], even i], [xs !! i | i <- [0..(length xs) - 1], odd i]]
