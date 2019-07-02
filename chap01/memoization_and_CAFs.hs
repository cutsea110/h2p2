-- | Eureka!
-- lift to top-level
-- >>> :sprint xs
-- xs = _
-- >>> fib_mem 0
-- 1
-- >>> :sprint xs
-- xs = 1 : _
-- >>> fib_mem 1
-- 1
-- >>> :sprint xs
-- xs = 1 : 1 : _
-- >>> fib_mem 2
-- 2
-- >>> :sprint xs
-- xs = 1 : 1 : 2 : _
-- >>> fib_mem 10
-- 89
-- >>> :sprint xs
-- xs = 1 : 1 : 2 : 3 : 5 : 8 : 13 : 21 : 34 : 55 : 89 : _

fib_mem :: Int -> Integer
fib_mem = (xs !!)

xs :: [Integer]
xs = map fib [0..]

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib_mem (n-2) + fib_mem (n-1)

fib_mem' :: Int -> Integer
fib_mem' = (map (\n -> if n <= 1 then 1 else fib_mem' (n-2) + fib_mem' (n-1)) [0..] !!)

------------------

fib_mem_arg :: Int -> Integer
fib_mem_arg = \i -> map (\n -> if n <= 1 then 1 else fib_mem_arg (n-2) + fib_mem_arg (n-1)) [0..] !! i
