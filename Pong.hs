{-# OPTIONS_GHC -Wall -Wunused-matches -Wname-shadowing #-}

import System.Console.ANSI
import System.IO
import System.Random
import System.Timeout

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Loops

import Debug.Trace

{-
,------.            ,--.                ,---.             ,--.    ,--------.
|  .-.  \  ,--,--.,-'  '-. ,--,--.     /  O  \ ,--,--,  ,-|  |    '--.  .--',--. ,--.,---.  ,---.  ,---.
|  |  \  :' ,-.  |'-.  .-'' ,-.  |    |  .-.  ||      \' .-. |       |  |    \  '  /| .-. || .-. :(  .-'
|  '--'  /\ '-'  |  |  |  \ '-'  |    |  | |  ||  ||  |\ `-' |       |  |     \   ' | '-' '\   --..-'  `)
`-------'  `--`--'  `--'   `--`--'    `--' `--'`--''--' `---'        `--'   .-'  /  |  |-'  `----'`----'
-}

type Vector = (Int, Int)

data Player = Player
  { position :: [Vector]
  , movement :: Vector
  , scoreAndLives :: Vector
  } deriving (Show)

data State = State
  { board :: Int
  , player1 :: Player
  , player2 :: Player
  , ball :: Vector
  , ballMovement :: Vector
  } deriving (Show)


-- The main loop

main :: IO State
main =
  clearScreen >> initialState initialPlayer1 initialPlayer2 >>=
  iterateUntilM gameOver step

{-


,------.  ,--.              ,--.                     ,------.                        ,--.  ,--.
|  .-.  \ `--' ,---.  ,---. |  | ,--,--.,--. ,--.    |  .---',--.,--.,--,--,  ,---.,-'  '-.`--' ,---. ,--,--,  ,---.
|  |  \  :,--.(  .-' | .-. ||  |' ,-.  | \  '  /     |  `--, |  ||  ||      \| .--''-.  .-',--.| .-. ||      \(  .-'
|  '--'  /|  |.-'  `)| '-' '|  |\ '-'  |  \   '      |  |`   '  ''  '|  ||  |\ `--.  |  |  |  |' '-' '|  ||  |.-'  `)
`-------' `--'`----' |  |-' `--' `--`--'.-'  /       `--'     `----' `--''--' `---'  `--'  `--' `---' `--''--'`----'

-}


displayState :: State -> IO State
displayState state = setCursorPosition 0 0 >> putStr (render state) >> return state

render :: State -> String
render state =
  unlines $
  applyBorder (board state) $ map (renderRow state) $ buildBoard (board state)

applyBorder :: Int -> [String] -> [String]
applyBorder size renderedRows =
  border ++ map (\row -> "|" ++ row ++ "|") renderedRows ++ border
  where
    border = [replicate (size + 2) '_']

renderRow :: State -> [Vector] -> String
renderRow state = map (characterForPosition state)

buildBoard :: Int -> [[Vector]]
buildBoard size =
  [ [ (x, y)
    | x <- [0 .. size - 1] ]
  | y <- [0 .. size - 1] ]

getPosition :: Player -> [Vector]
getPosition player = position player

characterForPosition :: State -> Vector -> Char
characterForPosition State {player1 = player1Variable
                           ,player2 = player2Variable
                           ,ball = currentBall} position
  | position `elem` getPosition player1Variable = '_'
  | position `elem` getPosition player2Variable = '_'
  | currentBall == position = 'O'
  | otherwise = ' '

getInput :: IO Char
getInput = hSetEcho stdin False >> hSetBuffering stdin NoBuffering >> getChar

-- Change this so that game over when out of lives, not when ball touches
gameOver :: State -> Bool
gameOver State {player1 = player1Variable
               ,player2 = player2Variable}
  | snd (scoreAndLives player1Variable) <= 0 || snd (scoreAndLives  player2Variable) <= 0 = True
  | otherwise = False

oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

initialState :: Player -> Player -> IO State
initialState player1Variable player2Variable =
  getStdGen >>=
  \stdGen ->
     return
       State
       { board = 30
       , player1 = player1Variable
       , player2 = player2Variable
       , ball = (5, 5)
       , ballMovement = ballMove (1, 1) 30.0
       }

initialPlayer1 =
  Player
  { position = [(27, 2), (28, 2), (29, 2)]
  , movement = (0, 0)
  , scoreAndLives = (0, 2)
  }

initialPlayer2 =
  Player
  { position = [(14, 28), (15, 28), (16, 28)]
  , movement = (0, 0)
  , scoreAndLives = (0, 2)
  }

getLastElement :: [Vector] -> Vector
getLastElement vectorArray = head (tail (tail vectorArray))

{-

,------. ,--.                                  ,------.                        ,--.  ,--.
|  .--. '|  | ,--,--.,--. ,--.,---. ,--.--.    |  .---',--.,--.,--,--,  ,---.,-'  '-.`--' ,---. ,--,--,  ,---.
|  '--' ||  |' ,-.  | \  '  /| .-. :|  .--'    |  `--, |  ||  ||      \| .--''-.  .-',--.| .-. ||      \(  .-'
|  | --' |  |\ '-'  |  \   ' \   --.|  |       |  |`   '  ''  '|  ||  |\ `--.  |  |  |  |' '-' '|  ||  |.-'  `)
`--'     `--' `--`--'.-'  /   `----'`--'       `--'     `----' `--''--' `---'  `--'  `--' `---' `--''--'`----'
                     `---'
-}
-- There is unnecessary code here, handle it when it works
playerCollisionWithBall :: Player -> Vector -> Bool
playerCollisionWithBall Player {position = playerPositionArray} ballPosition =
  playerBottom >= ballTop &&
  playerTop <= ballBottom && playerLeft <= ballRight && playerRight >= ballLeft
  where
    playerLeft = fst (head playerPositionArray)
    playerRight = fst (getLastElement playerPositionArray)
    playerTop = snd (head playerPositionArray)
    playerBottom = snd (getLastElement playerPositionArray)
    ballLeft = fst ballPosition
    ballRight = fst ballPosition
    ballTop = snd ballPosition
    ballBottom = snd ballPosition

incrementScore :: Player -> Player
incrementScore player@(Player {scoreAndLives = (score, lives)}) =
  player
  { scoreAndLives = (score + 1, lives)
  }

decrementLife :: Player -> Player
decrementLife player@(Player {scoreAndLives = (score, lives)}) =
  player
  { scoreAndLives = (score, lives - 1)
  }

handleBallCollisionWithPlayer :: State -> State
handleBallCollisionWithPlayer state@(State {player1 = player1Variable
                                           ,player2 = player2Variable
                                           ,ballMovement = ballMovementVariable
                                           ,ball = ballPosition})
  | playerCollisionWithBall player1Variable ballPosition =
    state
    { ballMovement = bounceHorizontal ballMovementVariable
    , player1 = incrementScore player1Variable
    }
  | playerCollisionWithBall player2Variable ballPosition =
    state
    { ballMovement = bounceHorizontal ballMovementVariable
    , player2 = incrementScore player2Variable
    }
  | otherwise = state

updatePlayerPosition :: Player -> Player
updatePlayerPosition player@(Player {movement = playerMovement
                                    ,position = playerPosition}) =
  player
  { position = applyMovement playerMovement playerPosition
  }

updatePlayers :: State -> State
updatePlayers state@(State {player1 = player1Variable
                           ,player2 = player2Variable}) =
  state
  { player1 = updatePlayerPosition player1Variable
  , player2 = updatePlayerPosition player2Variable
  }

updateIndividualPlayer :: Player -> Vector -> Player
updateIndividualPlayer player userInputMove =
  player
  { movement = userInputMove
  }

updateMovePlayer :: State -> Maybe Char -> State
updateMovePlayer state@(State {player1 = player1Variable
                              ,player2 = player2Variable}) userInputMove =
  state
  { player1 =
    updateIndividualPlayer player1Variable (player1InputChar userInputMove)
  , player2 =
    updateIndividualPlayer player2Variable (player2InputChar userInputMove)
  }

applyMovement :: Vector -> [Vector] -> [Vector]
applyMovement movement vector = map (move movement) vector


player1InputChar :: Maybe Char -> Vector
player1InputChar (Just 'a') = (-1, 0)
player1InputChar (Just 's') = (1, 0)
player1InputChar _ = (0, 0)

player2InputChar :: Maybe Char -> Vector
player2InputChar (Just 'k') = (-1, 0)
player2InputChar (Just 'l') = (1, 0)
player2InputChar _ = (0, 0)



{-
,-----.          ,--.,--.    ,------.                        ,--.  ,--.
|  |) /_  ,--,--.|  ||  |    |  .---',--.,--.,--,--,  ,---.,-'  '-.`--' ,---. ,--,--,  ,---.
|  .-.  \' ,-.  ||  ||  |    |  `--, |  ||  ||      \| .--''-.  .-',--.| .-. ||      \(  .-'
|  '--' /\ '-'  ||  ||  |    |  |`   '  ''  '|  ||  |\ `--.  |  |  |  |' '-' '|  ||  |.-'  `)
`------'  `--`--'`--'`--'    `--'     `----' `--''--' `---'  `--'  `--' `---' `--''--'`----'
-}
bounceVertical :: Vector -> Vector
bounceVertical (x, y) = (-x, y)

bounceHorizontal :: Vector -> Vector
bounceHorizontal (x, y) = (x, -y)

move :: Vector -> Vector -> Vector
move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

ballHittingTopWall :: State -> Bool
ballHittingTopWall State {board = boardSize
                         ,ball = (currentBall@(_, ballY))}
  | ballY > boardSize - 2 = True
  | otherwise =  False

-- todo never getting to this point
ballHittingBottomWall State {ball = (_, ballY)}
  | ballY - 2 <= 0 = True
  | otherwise =  False

ballHittingVerticalWall :: State -> Bool
ballHittingVerticalWall State {board = boardSize
                                ,ball = (currentBall@(ballX, _))}
  | ballX > boardSize - 2 = True
  | ballX - 2 <= 0 = True
  | otherwise = False

moveVertical :: Vector -> Vector -> Vector
moveVertical (x1, y1) (x2, y2) = (x1 - x2, y1 + y2)

moveHorizontal :: Vector -> Vector -> Vector
moveHorizontal (x1, y1) (x2, y2) = (x1 + x2, y1 - y2)

ballMove :: Vector -> Float -> Vector
ballMove (dx, dy) degree =
  ( ceiling ((fromIntegral dx :: Float) * c + s * (fromIntegral dy :: Float))
  , ceiling ((fromIntegral dx :: Float) * (-s) + c * (fromIntegral dy :: Float)))
  where
    radians = degree * (pi / 180)
    s = sin radians
    c = cos radians

-- TODO make this random at some point and include the angle of movement
-- ballReset :: Vector
-- ballReset vector =  (5, 5)

updateBall :: State -> State
updateBall state@(State {ballMovement = vector
                        ,player1 = player1Variable
                        ,player2 = player2Variable})
  | ballHittingVerticalWall state =
    state
    { ballMovement = bounceVertical vector
    , ball = ball state `moveVertical` vector
    }
  | ballHittingTopWall state =
    state
    { ball =  (15,15)
    , player1 = decrementLife player1Variable
    , ballMovement = ballMove (1, 1) 30.0
    }
  | ballHittingBottomWall state =
  state
  { ball = (15,15),
    player2 = decrementLife player2Variable,
    ballMovement = ballMove (1, 1) 30.0
  }
  | otherwise =
    state
    { ball = ball state `move` vector
    }


{-

,--.   ,--.,--.                 ,------.                        ,--.  ,--.
|   `.'   |`--' ,---.  ,---.    |  .---',--.,--.,--,--,  ,---.,-'  '-.`--' ,---. ,--,--,  ,---.
|  |'.'|  |,--.(  .-' | .--'    |  `--, |  ||  ||      \| .--''-.  .-',--.| .-. ||      \(  .-'
|  |   |  ||  |.-'  `)\ `--.    |  |`   '  ''  '|  ||  |\ `--.  |  |  |  |' '-' '|  ||  |.-'  `)
`--'   `--'`--'`----'  `---'    `--'     `----' `--''--' `---'  `--'  `--' `---' `--''--'`----'

-}

--         todo make a ballResetfunction
--       todo check if this is player 1 or player2
step :: State -> IO State
step state =
  sample sampleLength getInput >>=
  \userInputMove -> displayState $ updateState state userInputMove

updateState :: State -> Maybe Char -> State
updateState state userInputMove =
  handleBallCollisionWithPlayer $
  updateBall $ updatePlayers $ updateMovePlayer state userInputMove

sample :: Int -> IO a -> IO (Maybe a)
sample sampleLength getInput
  | sampleLength < 0 = fmap Just getInput
  | sampleLength == 0 = return Nothing
  | otherwise =
    concurrently (timeout sampleLength getInput) (threadDelay sampleLength) >>=
    \(result, _) -> return result
