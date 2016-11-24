-- Dentro de Data.List já existe uma função chamada transpose, porém não
-- é carregada no Prelude
transpose			:: [[Char]] -> [[Char]]
transpose []			= []
transpose ([]:xss) 		= transpose xss
transpose ((x:xs):xss) 		= (x:[h | (h:_) <- xss]) : transpose (xs:[t | (_:t) <- xss])
