module GameLogic where

import Board (..)
import Block (..)
import Random (..)
import Dict
import Helpers (..)
import List (map, member, (::), filter, length, partition, concatMap, concat)
import Location (..)
import Debug

type alias GameState = 
  { board : Board
  , upper : Maybe Block
  , lower : Maybe Block
  , cursor : Location
  , seed : Seed
  }

getClusters : Board -> List (List (Int, Int))
getClusters (Board b) =
    concatMap (\x -> getClusters' (snd x)) <| Debug.watch "ByColor" <| groupByColor <| Dict.toList b

getClusters' : List (Int, Int) -> List (List (Int, Int))
getClusters' xs =
    case xs of
        [] -> []
        (loc :: locs) -> let (cluster, rest) = getClusterFromLoc loc locs in
            cluster :: (getClusters' rest)

getClusterFromLoc : (Int, Int) -> List (Int, Int) -> (List (Int, Int), List (Int, Int))
getClusterFromLoc loc locs =
    repeatUntilFix growCluster ([loc], locs)

growCluster (clus, res) =
    let possibleNeighbors = nub (concatMap (\c -> map (locAdd c) neighbors) clus) in
    let (existingNeighbors, notNeighbors) = partition ((flip member) possibleNeighbors) res in
    (clus ++ existingNeighbors, notNeighbors)

removeCluster : GameState -> GameState
removeCluster gs = 
    let toRemove = (concat <| filter (\xs -> length xs > 2) (getClusters gs.board)) in
    { gs | board <- gravity (nub (map (\(x,y) -> x) toRemove)) <| deleteMultiple toRemove gs.board }
