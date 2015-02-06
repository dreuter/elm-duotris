module Board where

import Constants (..)
import Graphics.Collage (..)
import Color (..)

import Dict (Dict)
import Dict

import List (..)

import Block (Block, toSprite)
import Location (..)

import Helpers (..)

type Board = Board (Dict Location Block)

emptyBoard : Board
emptyBoard = Board Dict.empty

-- asElement : Board -> Element
asElement (Board b) =
    let width = blockSize*boardWidth in
    let height = 2*blockSize*boardHeight in
    let background = filled black <| rect (toFloat width) (toFloat height) in
    let blocks = Dict.foldr (\(x,y) block xs -> (move (toFloat x * blockSize, toFloat y * blockSize) <| toSprite block) :: xs) [] b in
    let spacer = filled grey <| rect (toFloat width) (toFloat blockSize) in
    collage width height <| [background, spacer] ++ blocks

maybeInsert : Location -> Maybe Block -> Board -> Board
maybeInsert key mval (Board dict) =
  case mval of 
    Just val -> Board <| Dict.insert key val dict
    _ -> Board dict

isBlocked : Location -> Board -> Bool
isBlocked (x,y) (Board b) = if y == 0
    then True
    else Dict.member (x,y) b

insert loc val (Board dict) = Board <| Dict.insert loc val dict

delete loc (Board dict) = Board <| Dict.remove loc dict

deleteMultiple : List (Int, Int) -> Board -> Board
deleteMultiple locs (Board dict) = Board <| foldr Dict.remove dict locs

anyInTopRow (Board b) =
    any (\((_,y),_) -> (boardHeight - 1) == abs y ) <| Dict.toList b

get : Location -> Board -> Maybe Block
get loc (Board b) = Dict.get loc b

gravity : List Int -> Board -> Board
gravity rows board = foldr gravityOneRow board rows


gravityOneRow : Int -> Board -> Board
gravityOneRow r (Board b) =
    let newOldPosPositive = map (\(new, old) -> ((r, old),(r, new))) <| indexedMap (\x y -> (x+1, y)) <| sort <| map (\((x,y),_) -> y) <| filter (\((x,y),_) -> x == r && y > 0) (Dict.toList b) in
    let newOldPosNegative = map (\(new, old) -> ((r, old),(r, new))) <| indexedMap (\x y -> (-x-1, y)) <| reverse <| sort <| map (\((x,y),_) -> y) <| filter (\((x,y),_) -> x == r && y < 0) (Dict.toList b) in
        moveMultiple (reverse newOldPosPositive ++ reverse newOldPosNegative) (Board b)

pop : Location -> Board -> (Block, Board)
pop loc b = (fromJust <| get loc b, delete loc b)

moveBlock : Location -> Location -> Board -> Board
moveBlock old new b = let (block, b') = pop old b in
    insert new block b'

moveMultiple : List (Location, Location) -> Board -> Board
moveMultiple moves b = foldr (\(old, new) b' -> moveBlock old new b') b moves
