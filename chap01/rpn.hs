data Lex = Number Double Lex
         | Plus Lex
         | Times Lex
         | End
lexRPN :: String -> Lex
lexRPN = go . words
  where go ("*":rest) = Times (go rest)
        go ("+":rest) = Plus (go rest)
        go (num:rest) = Number (read num) (go rest)
        go         [] = End

evalRPN :: Lex -> Double
evalRPN = go []
  where
    go stack (Number num rest)    = go (num:stack) rest
    go (o1:o2:stack) (Plus rest)  = let r = o1 + o2 in r `seq` go (r:stack) rest
    go (o1:o2:stack) (Times rest) = let r = o1 * o2 in r `seq` go (r:stack) rest
    go [res] End                  = res
