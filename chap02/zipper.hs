type Zipper a = ([a], [a])

forward, backward :: Zipper a -> Zipper a
forward (xs, y:ys) = (y:xs, ys)
backward (x:xs, ys) = (xs, x:ys)

get :: Zipper a -> a
get (_, y:_) = y

set :: a -> Zipper a -> Zipper a
set x (xs, _:ys) = (xs, x:ys)
