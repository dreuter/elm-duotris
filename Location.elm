module Location where

type alias Location = (Int, Int)

locAdd (x, y) (x', y') = (x+x', y+y')

negateLocation (x, y) = (x, -y)

neighbors = [(-1, 0), (0, -1), (0, 1), (1, 0)]