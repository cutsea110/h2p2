sma :: [Double] -> [Double]
sma (x0:x1:xs) = (x0 + x1) / 2 : sma (x1:xs)
sma xs         = xs

main = let a = [1..1000000]
           b = sma a
           c = sum b
           in print c
