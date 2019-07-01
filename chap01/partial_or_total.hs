partialHead :: [a] -> a
partialHead (x:_) = x

totalHead :: [a] -> Maybe a
totalHead []    = Nothing
totalHead (x:_) = Just x

partial = print $ partialHead [1..]

total = print $ case totalHead [1..] of
                  Nothing -> 1
                  Just n  -> n

-- main = partial
main = total
