data Nota 					= Nota Float Float Float deriving (Show)
data Aluno 					= Aluno [Char] Nota deriving (Show)

media 						:: Aluno -> Float
media (Aluno _ (Nota n1 n2 n3)) 		= (n1 + n2 + n3) / 3

ordena 						:: [Aluno] -> [Aluno]
ordena [] 					= []
ordena ((Aluno n nn):xs) 			= [(Aluno m nm) | (Aluno m nm) <- xs, m <= n]
						++ [(Aluno n nn)] ++
						[(Aluno m nm) | (Aluno m nm) <- xs, m > n]

ordenaMedia 					:: [Aluno] -> [([Char], Float)]
ordenaMedia xs 					= [(n, media (Aluno n nn)) | (Aluno n nn) <- ordena xs]
