{-# OPTIONS_GHC -Wall #-}

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
    player2 :: [Vector],
    ball :: Vector,
    move  :: Maybe Vector
} deriving Show


main :: IO State
main = clearScreen
    >> initialState
    >>= (iterateUntilM gameOver step)

vectorFromChar :: Maybe Char -> Maybe Vector
vectorFromChar (Just 'w') = Just ( 0,  1)
vectorFromChar (Just 's') = Just ( 0, -1)
vectorFromChar (Just 'l') = Just ( 0, -1)
vectorFromChar (Just 'o') = Just ( 0,  1)
vectorFromChar _          = Nothing

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

characterForPosition :: State -> Vector -> Char
characterForPosition state position
    | position `elem` player1 state            = '#'
    | position `elem` player2 state            = '#'
    | ball state `ballPositionEquals` position = '@'
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
--     | ballX >= boardSize || ballX < 0 = True
--     | ballY >= boardSize || ballY < 0 = True
    | otherwise                       = False


oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

initialState :: IO State
initialState = getStdGen
    >>= \stdGen -> return State {
        board = 15,
        player1 = [(1,4),(1,5),(1,6)],
        player2 = [(10,4),(10,5),(10,6)],
        ball = (5,5),
        move = Just (2,1)
    }

bounce :: Vector -> Vector
bounce (x, y) = (-x,y)

updateMove :: State -> Maybe Vector -> State
updateMove state@(State { move = Just vector }) inputMove@(Just inputVector)
    | inputVector == vectorOpposite vector  = state
    | otherwise = state { move = inputMove <|> move state }
updateMove state _ = state

vectorOpposite :: Vector -> Vector
vectorOpposite (x, y) = (-x, -y)

applyMove :: Vector -> Vector -> Vector
applyMove (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

updateBall :: State -> State
updateBall state@(State{ move = (Just vector)}) =
 state { ball = ball state `applyMove` vector }
-- updateBall state = state

buildBoard :: Int -> [[(Int,Int)]]
buildBoard size = [[(x, y) | x <- [0 .. size - 1]] | y <- reverse [0 .. size - 1 ]]


step :: State -> IO State
step state = sample sampleLength getInput 
    >>= \ inputMove ->
        displayState $ updateState state (vectorFromChar inputMove)

updateState :: State -> Maybe Vector -> State
updateState state inputMove
    = updateBall state


sample :: Int -> IO a -> IO (Maybe a)
sample sampleLength getInput
    | sampleLength <  0    = fmap Just getInput
    | sampleLength == 0    = return Nothing
    | otherwise =
        concurrently (timeout sampleLength getInput) (threadDelay sampleLength) 
            >>= \ (result, _) -> return result