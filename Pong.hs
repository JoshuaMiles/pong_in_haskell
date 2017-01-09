{-# OPTIONS_GHC -Wall -Wunused-matches #-}

import Data.List

import System.IO
import System.Timeout
import System.Random
import System.Console.ANSI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Loops
import Control.Applicative


type Vector = (Int, Int)

data State = State {
    board :: Int,
    player1 :: [Vector],
    player1Movement :: Maybe Vector,
    player2 :: [Vector],
    player2Movement :: Maybe Vector,
    ball :: Vector,
    ballMovement  :: Maybe Vector
} deriving Show


main :: IO State
main = clearScreen
    >> initialState
    >>= (iterateUntilM gameOver step)

userInputChar :: Maybe Char -> Maybe Vector
userInputChar (Just 'a')  = Just ( -1,  0)
userInputChar (Just 's') = Just ( 1, 0)
userInputChar (Just 'k') = Just ( -1, 0)
userInputChar (Just 'l') = Just ( 1,  0)
userInputChar _          = Just ( 0, 0)

displayState :: State -> IO State
displayState state = setCursorPosition 0 0 
    >> putStr (render state) 
    >> return state

render :: State -> String
render state = unlines $ applyBorder (board state)
                       $ map (renderRow state)
                       $ buildBoard (board state)

applyBorder :: Int -> [String] -> [String]
applyBorder size renderedRows
    = border ++ map (\row -> "|" ++ row ++ "|") renderedRows ++ border
        where border = [replicate ( size + 2 ) '_' ]

renderRow :: State -> [Vector] -> String
renderRow state = map (characterForPosition state)

buildBoard :: Int -> [[Vector]]
buildBoard size = [[(x, y) | x <- [0 .. size - 1]] | y <- [0 .. size - 1 ]]

characterForPosition :: State -> Vector -> Char
characterForPosition state position
    | position `elem` player1 state            = '_'
    | position `elem` player2 state            = '_'
    | ball state `ballPositionEquals` position = 'O'
    | otherwise                                = ' '

ballPositionEquals :: Vector -> Vector -> Bool
ballPositionEquals position vector = position == vector
ballPositionEquals _ _                         = False


getInput :: IO Char
getInput = hSetEcho stdin False
    >> hSetBuffering stdin NoBuffering
    >> getChar

gameOver :: State -> Bool
gameOver (State {
    board = boardSize,
    ball = (currentBall@(ballX, ballY))
})
    | ballY >= boardSize || ballY < 0 = True
    | otherwise                       = False


oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

initialState :: IO State
initialState = getStdGen
    >>= \stdGen -> return State {
        board = 30,
        player1 = [((ceiling(30/2)-1),2),(ceiling(30/2),2),((ceiling(30/2)+1),2)],
        player1Movement = Just (0,0),
        player2 = [(ceiling(30/2)-1,28),(ceiling(30/2),28),(ceiling(30/2)+1,28)],
        player2Movement = Just (0,0),
        ball = (5,5),
        ballMovement = Just (1,1)
    }

bounce :: Vector -> Vector
bounce (x, y) = (-x,y)

-- Need to distinguish between player 1 and player 2
updateMove :: State -> Maybe Vector -> State
updateMove state userInputMove@(Just inputVector) =
    state {
        player1Movement = userInputMove <|> player1Movement state,
        player2Movement = userInputMove <|> player2Movement state
        }
updateMove state _ = state

vectorOpposite :: Vector -> Vector
vectorOpposite (x, y) = (-x, -y)

move :: Vector -> Vector -> Vector
move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

applyMovement :: Vector -> [Vector] -> [Vector]
applyMovement movement vector =
    map (move movement) vector


checkIfBallIsTouchingWall :: State -> Bool
checkIfBallIsTouchingWall (State {
                              board = boardSize,
                              ball = (currentBall@(ballX, ballY))
                          })
                          | ballX >= boardSize || ballX <= 0  = True
                          | ballY >= boardSize || ballY >= 0 = True
                          | otherwise = False

updateBall :: State -> State
updateBall state@(State{ ballMovement = (Just vector)})
    | checkIfBallIsTouchingWall state = state { ball = ball state `move` bounce vector }
    | otherwise = state { ball = ball state `move` vector }
-- updateBall state = state

updatePlayers :: State -> State
updatePlayers state@(State{ player1Movement = Just player1Vector, player2Movement = Just player2Vector}) =
    state { player1 = applyMovement player1Vector (player1 state), player2 = applyMovement player2Vector (player2 state)}

step :: State -> IO State
step state = sample sampleLength getInput 
    >>= \ userInputMove ->
        displayState $ updateState state (userInputChar userInputMove)

updateState :: State -> Maybe Vector -> State
updateState state userInputMove
    = updateBall $ updatePlayers $ updateMove state userInputMove


sample :: Int -> IO a -> IO (Maybe a)
sample sampleLength getInput
    | sampleLength <  0    = fmap Just getInput
    | sampleLength == 0    = return Nothing
    | otherwise =
        concurrently (timeout sampleLength getInput) (threadDelay sampleLength) 
            >>= \ (result, _) -> return result