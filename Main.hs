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

vectorFromChar :: Maybe Char -> Maybe Vector
vectorFromChar (Just 'w') = Just ( 0,  1)
vectorFromChar (Just 's') = Just ( 0, -1)
vectorFromChar (Just 'l') = Just ( 0, -1)
vectorFromChar (Just 'o') = Just ( 0,  1)
vectorFromChar _          = Nothing

render :: State -> String
render state = unlines $ applyBorder (board state)
                       $ map (renderRow state)
                       $ buildBoard (board state)

applyBorder :: Int -> [String] -> [String]
applyBorder size renderedRows
    = border ++ map (\row -> "X" ++ row ++ "X") renderedRows ++ border
        where border = [replicate ( size + 2 ) 'X' ]

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

applyMove :: Vector -> Vector -> Vector
applyMove (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

bounce :: Vector -> Vector
bounce (x, y) = (-x,y)

updateBall :: State -> State
updateBall state@(State{ move = (Just vector)})
    = state { ball = ball state `applyMove` move state }
updateBall state = state

buildBoard :: Int -> [[(Int,Int)]]
buildBoard size = [[(x, y) | x <- [0 .. size - 1]] | y <- reverse [0 .. size - 1 ]]