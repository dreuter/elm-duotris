import Graphics.Element (..)
import Signal
import Signal (Signal)
import Keyboard
import Text
import Time
import Window

import Maybe
import Random (generate, Seed, initialSeed)

import Board (..)
import Block (..)
import Color (..)
import Constants (..)
import Location (..)
import GameLogic (..)

import Debug

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type Input = Move Move | DropHard | Pause | Step

type Move = NoMovement | Left | Right | Down

todLoc m = case m of
    NoMovement -> (0, 0)
    Left -> (-1, 0)
    Right -> (1, 0)
    Down -> (0, -1)


{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}

defaultGame : GameState
defaultGame = 
    let s = initialSeed 21 in
    let (upper, s') = generate randomBlock s in
    let (lower, s'') = generate randomBlock s' in
      { board = emptyBoard
      , upper = Just upper
      , lower = Just lower
      , cursor = (0, boardHeight)
      , seed = s''
      }



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame inp gameState = 
    Debug.watch "GameState" <|
    removeCluster <|
    checkGameOver <| replaceLost <| checkDropped <|
    case inp of
        Step -> let (x,y) = gameState.cursor in { gameState | cursor <- (x, y-1)}
        Move m ->
          let newPos = locAdd gameState.cursor <| todLoc m in
          let b = gameState.board in
          let gs = gameState in
          if isJust gs.upper && isBlocked newPos b
            || isJust gs.lower && isBlocked (negateLocation newPos) b
            then gameState
            else { gameState | cursor <- newPos }
        _    -> gameState

isNothing m = case m of
    Nothing -> True
    _ -> False

isJust m = case m of
    Just _ -> True
    Nothing -> False

checkGameOver gs =
    if anyInTopRow gs.board
       then defaultGame
       else gs

replaceLost gs =
    if isNothing gs.upper && isNothing gs.lower
        then
            let (x,y) = gs.cursor in
            let (upper, s') = generate randomBlock gs.seed in
            let (lower, s'') = generate randomBlock s' in
                { gs | cursor <- (x, boardHeight), seed <- s'', upper <- Just upper, lower <- Just lower }
        else gs


checkDropped : GameState -> GameState
checkDropped gs =
    (
    applyOrDefault gs.upper gs <| \upper ->
    let (x, y) = gs.cursor in
        if isBlocked (x, y-1) gs.board
            then { gs | board <- insert (x,y) upper gs.board, upper <- Nothing }
            else gs
    )
    |> \ gs' ->
    applyOrDefault gs'.lower gs' <| \lower ->
    let (x, y) = gs'.cursor in
        if isBlocked (x, 1-y) gs.board
            then { gs' | board <- insert (x, -y) lower gs'.board, lower <- Nothing }
            else gs'


applyOrDefault m default f =
    case m of
        Just a -> f a
        _ -> default

{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) gs =
    asElement {
        gs | board <- maybeInsert gs.cursor gs.upper 
            <| maybeInsert (negateLocation gs.cursor) gs.lower gs.board
        }.board


{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}


keyInput =
    Keyboard.directions keys.up keys.down keys.left keys.right
    |> \s -> Signal.merge s (Signal.sampleOn (Time.every  <| 0.3 * Time.second) s)
    |> Signal.map (
          \{x, y} ->
              case x of
                  -1 -> Move Left
                  1 -> Move Right
                  0 -> case y of
                      -1 -> Move Down
                      _  -> Move NoMovement
        )

input : Signal Input
input =
    Signal.merge keyInput <| Signal.map (\_ -> Step) <| Time.every Time.second

gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


main : Signal Element
main =
    Signal.map2 display Window.dimensions gameState
