sma :: [Double] -> [Double]
sma (x0:x1:xs) = (x0+x1)/2:sma (x1:xs)
sma        xs  = xs
