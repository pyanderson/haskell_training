-- o Prelude carrega uma função chamada gcd que possui mesmo funcionamento
mmc'            :: (Integral a) => a -> a -> a
mmc' x 0        = x
mmc' x y        = mmc' y (x `rem` y)

mmc             :: (Integral a) => a -> a -> a
mmc x y         =  mmc' (abs x) (abs y)

