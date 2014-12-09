module GameOfLife.Board where

import System.IO

import GameOfLife.Position
import GameOfLife.Helpers

width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

-- glider: an initial board (left to right, up and down)
--
-- |     |     |     |     |     |
-- |     |     |     |(4,2)|     |
-- |     |(2,3)|     |(4,3)|     |
-- |     |     |(3,4)|(4,4)|     |
-- |     |     |     |     |     |
--
glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

-- showCells: display the living cells on the screen
showCells       :: Board -> IO ()
showCells board = seqn [writeAt pos "O" | pos <- board]

-- isAlive: True if the given position on the board is alive
isAlive           :: Board -> Pos -> Bool
isAlive board pos = elem pos board

-- isEmpty: True if there's no alive cell in the given position on the board
isEmpty           :: Board -> Pos -> Bool
isEmpty board pos = not (isAlive board pos)

-- neighbs: returns the 8 neighbours of a position
--          wrap takes account of the wrapping around at the edges of the board
neighbs       :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1), (x+1,y-1),
                          (x-1,y),            (x+1,y),
                          (x-1,y+1), (x,y+1), (x+1,y+1)]
                where
                    wrap       :: Pos -> Pos
                    wrap (x,y) = ( ((x-1) `mod` width)  + 1,
                                   ((y-1) `mod` height) + 1 )

-- liveNeighbs: calculates the number of live neighbours for a given position
liveNeighbs       :: Board -> Pos -> Int
liveNeighbs board = length . filter (isAlive board) . neighbs

-- survivors: produces the list of living positions in a board
--            that have precisely 2 or 3 living neighbours
--            (and hence survive to the next generation)
survivors       :: Board -> [Pos]
survivors board = [pos | pos <- board, elem (liveNeighbs board pos) [2,3]]

-- births: produces the list of empty positions in a board
--         that have precisely 3 living neighbours
--         (and hence give birth to a new cell)
births       :: Board -> [Pos]
births board = [(x,y) | x <- [1..width], y <- [1..height],
                        isEmpty board (x,y),
                        liveNeighbs board (x,y) == 3]

-- nextGen: produces the next generation of a board
nextGen       :: Board -> Board
nextGen board = survivors board ++ births board

-- life: runs the Game of Life
life       :: Board -> IO ()
life board = cls >>
             showCells board >>
             wait 100000 >>
             life (nextGen board)

