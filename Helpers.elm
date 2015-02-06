module Helpers where

import List ((::), partition, map, sortBy)
import Block (..)
import Debug

groupBy xs = case xs of
    [] -> []
    ((k, x) :: xs') -> let (eq, res) = partition (\x -> fst x == k) xs' in
        [(k, x :: (map snd eq))] ++ groupBy res

groupByColor l = groupBy <| map (\(loc, (Block c p)) -> (c, loc)) l

repeatUntilFix f x =
    if f x == x
        then x
        else repeatUntilFix f (f x)

nub a = case a of
    (x :: xs) -> let (eq, res) = partition ((==) x) xs in x :: (nub res)
    [] -> []

fromJust : Maybe a -> a
fromJust m = case m of
    Just x -> x
    _ -> Debug.crash "Called fromJust with Nothing"