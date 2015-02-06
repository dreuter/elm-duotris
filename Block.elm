module Block where

import Constants (blockSize)
import Color (..)
import Graphics.Collage (..)
import Random (..)
import List

type PowerUp = None

colors = [red, blue, green, orange, yellow, purple]

type Block = Block Color PowerUp

toSprite (Block c p) = case p of
    None -> filled c <| rect blockSize blockSize

randomBlock = customGenerator <| \s ->
    let (x, s') = generate (int 0 ((List.length colors) - 1)) s in
        (Block (List.head <| List.drop x colors) None, s')