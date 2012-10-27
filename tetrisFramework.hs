{- 
   Steffi Ostrowski
   Tetris!  
-}

import Random
import DrawX
import SOE hiding (Region)         -- use "SOE" for windows & linux
import qualified SOE as G (Region) -- here too
import PictureEX
import Data.List


columns = 10                         -- Columns in the board
rows = 15                            -- Number of rows in the board
squareSize = 30                      -- Square side length in pixels
winX = round (columns * squareSize)  -- Width of drawing window in pixels
winY = round (rows * squareSize)     -- Height of drawing window in pixels
speed = 500        -- speed/1000 is the number of seconds to move 1 unit in y.

-- Opens a buffered user window for animation using user coordinates.
-- Coordinates are min and max X, min and max Y in user coordinates
-- and width and height of the window in pixels.  Also widow title.
openUserWindowEx :: Float -> Float -> Float -> Float -> Int -> Int -> String -> IO UserWindow
openUserWindowEx uXmin uXmax uYmin uYmax winX winY title = 
  do w <- openWindowEx title (Just (0,0)) (Just (winX,winY)) drawBufferedGraphic
     return (WinData w uXmin (uXmax - uXmin) uYmax (uYmax - uYmin) winX winY)

-- Starts a new piece in a random column.
newPiece :: UserWindow -> Region ->  IO ()
newPiece uw board = do
  col <- randomRIO (3, round(columns)-2 :: Int)
  stime <- timeGetTime -- Gets system time in milliseconds
  num <- randomRIO (1,4 :: Int)
  let color = if num == 1
                then Red
                else if num==2
                       then Blue
                       else if num==3
                              then Green
                              else Yellow
  piece <- randomRIO (1, 7 :: Int)
  case piece of 
    1 -> loop uw (intToFloat col) stime rod board color
    2 -> loop uw (intToFloat col) stime block board color
    3 -> loop uw (intToFloat col) stime tPiece board color
    4 -> loop uw (intToFloat col) stime lPiece board color
    5 -> loop uw (intToFloat col) stime jog board color
    6 -> loop uw (intToFloat col) stime jogM board color
    7 -> loop uw (intToFloat col) stime lPieceM board color
  

-- Loop to control and display piece as it drops.  
-- x is current x coordinate; stime is time when piece was started (in new).
loop :: UserWindow -> Float -> Word32 -> Region -> Region -> Color -> IO ()
loop uw x stime piece oldBoard color = do
  clearWindow (win uw)
  time <- timeGetTime
  let y = rows + 1 - (intToFloat (time - stime))/speed  -- position is a function of time
  drawPic uw (Region color (Translate (x, y) piece))
  let board = deleteFullRows oldBoard
  drawShapesInWindow uw board
  e <- maybeGetWindowEvent (win uw)                      
  case e of Just Closed -> return ()
            Just (Key 'q' True) -> closeWindow (win uw)
            Just (Key 'j' True) -> loop uw (left piece x y board) stime piece board color
            Just (Key 'k' True) -> loop uw x stime (rotateL x y piece board) board color
            Just (Key 'l' True) -> loop uw (right piece x y board) stime piece board color
            Just (Key ' ' True) -> newPiece uw (dropPiece piece x y board) -- Drop the piece 
            _                   -> if hitCeiling board
            					       then gameOver uw stime board
            					       else if (hitBottom (Translate (x,y) piece)) || (hitBoard (Translate (x,y) piece) board)    
                                     then newPiece uw (addToBoard piece x (fromIntegral (ceiling y)) board)
                                     else loop uw x stime piece board color
                                     
                                     
-- Displays a message that says game over
gameOver :: UserWindow -> Word32 -> Region -> IO ()
gameOver uw stime board = do
  clearWindow (win uw)
  time <-timeGetTime
  drawShapesInWindow uw Red board
  --let shape = withColor Green (ellipse (round((intToFloat winX)/(intToFloat 2)),round((intToFloat winY)/(intToFloat 2))) (100,50))
  let background = Translate ((columns+1)/2, rows/2) (Shape (Ellipse 4 2))
  drawPic uw (Region Green background)
  e <- maybeGetWindowEvent (win uw)
  case e of Just Closed -> return ()
            Just (Key 'q' True) -> closeWindow (win uw)
            Just (Key 'n' True) -> newGame uw 
            _ -> gameOver uw stime board

-- starts a new game            
newGame :: UserWindow -> IO ()
newGame uw = newPiece uw (Shapes [])
  
                                     
-- Moves left if there is space
left piece x y board = let shifted = Translate (x-1,y) piece
					   in if (hitBoard shifted board || (Translate (x,y) piece) `containsCol` 1) then x else x-1

-- checks if a piece is in a particular column
containsCol :: Region -> Float -> Bool
r `containsCol` c = foldl (||) False (map (\y -> r `containsR` (c,y)) [1..rows])

-- returns true if the piece has hit the bottom 
hitBottom piece = piece `containsRow` (1/2)

-- checks if a piece is in a particular row
containsRow :: Region -> Float -> Bool
p `containsRow` r = foldl (||) False (map (\x -> p `containsR` (x,r)) [1..columns])


-- drops piece to its final position (where it hits the board or the bottom of the window
dropPiece :: Region -> Float -> Float -> Region -> Region
dropPiece piece x y board = let shifted = Translate (x,y) piece
					        in if hitBoard shifted board || hitBottom shifted
					               then addToBoard piece x (fromIntegral (ceiling y)) board
					               else dropPiece piece x (y-1) board

-- returns true if the piece has hit the board
hitBoard :: Region -> Region -> Bool
hitBoard piece board = foldl (||) False (map (\b -> board `containsBottomEdge` b) (getList (flattenBlocks piece)))

-- returns true if the board contains a point on the bottom edge of a block
r `containsBottomEdge` (Block (x,y) radius) = r `containsR` (x,y-radius)

-- checks to see if the board has hit the ceiling
hitCeiling board = foldl (||) False (map (\b -> snd (getLoc b) >= rows) (getList board))

-- gets the centroid out of a block
getLoc (Block v r) = v

-- returns true of a region contains a certain block
r `containsBlock` (Block (x,y) radius) = r `containsR` (x+radius,y)

--findFullRows board = deleteRows (map (\r ->(r,isFull r board)) [1..rows]) board
deleteFullRows :: Region -> Region
deleteFullRows board = deleteFullRowsHelper board [1..rows]

deleteFullRowsHelper :: Region -> [Float] -> Region
deleteFullRowsHelper board [] = board
deleteFullRowsHelper board (r:rs) = if (isFull r board)
							            then deleteFullRowsHelper (deleteRow r board) (r:rs)
							            else deleteFullRowsHelper board rs

-- determines if a certain row is full
isFull r board = foldl (&&) True (map (\x -> board `containsR` (x,r)) [1..columns])

-- deletes any and all full rows and shifts
deleteRow :: Float -> Region -> Region
deleteRow r (Shapes board) = let newBoard = filter (blockNotInRow r) board
					         in Shapes (shiftRows r newBoard)

-- shifts down all blocks in rows greater than r	
shiftRows :: Float -> [Shape] -> [Shape]
shiftRows r board = shiftRowsHelper r board []
					where shiftRowsHelper row [] newBoard = newBoard
					      shiftRowsHelper row ((Block (x,y) rad):bs) newBoard = if y>row
					      													        then shiftRowsHelper row bs ((Block (x,y-1) rad):newBoard)
					      													        else shiftRowsHelper row bs ((Block (x,y) rad):newBoard)

-- returns True if a block is in a given row
blockNotInRow :: Float -> Shape -> Bool
blockNotInRow r (Block (x,y) _) = not (r==y)

-- Moves right if there is space
right piece x y board = let shifted = Translate (x+1,y) piece
					    in if (hitBoard shifted board || (Translate (x,y) piece) `containsCol` 10) then x else x+1


-- Rotates a shape 90 degrees to the left unless the rotation would put it off the board
rotateL x y piece board = let shifted = Translate (x,y) (RotateL piece)
				          in if shifted `containsCol` 0 || shifted `containsCol` 11 || hitBoard shifted board
			                  then piece
			                  else RotateL piece

-- adds a piece to the board	
addToBoard piece x y Empty = Shapes ([]++getList (flattenBlocks (Translate (x,y) piece)))
addToBoard piece x y (Shapes board) = Shapes (board ++ getList (flattenBlocks (Translate (x,y) piece)))

-- gets the list of shapes out of the region
getList (Shapes list) = list

-- Run the game by starting a new piece falling
main = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Tetris"
     newPiece uw (Shapes [])
     
-- tests hitBottom, shows a piece that has hit the bottom    
test1 = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Test 1"
     let piece = Translate (1,1) rod
     drawRegionInWindow uw Red piece
     print $ hitBottom piece
     spaceClose (win uw)
 
-- test hitBottom, shows a piece that has not hit the bottom 
test2 = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Test 2"
     let piece = Translate (5,5) rod
     drawRegionInWindow uw Red piece
     print $ hitBottom piece
     spaceClose (win uw)

-- tests side-by-side board collision     
test3 = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Test 3"
     let piece1 = Translate (5,2) rod
     let piece2 = RotateL rod
     let board = addToBoard piece2 7 3 Empty
     drawRegionInWindow uw Blue piece1
     drawShapesInWindow uw Red board
     print $ hitBoard piece1 board
     spaceClose (win uw)
     
-- tests game over
test4 = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Test 4"
     let board = fillBoard 4 3 (Shapes [])
     drawShapesInWindow uw Red board
     print $ hitCeiling board
     spaceClose (win uw)
     
fillBoard n y board = if n>=1
                        then fillBoard (n-1) (y+4) (addToBoard (RotateL rod) 7 y board)
                        else board
                        
-- tests hitting the board
test5 = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Test 5"
     let board = addToBoard (RotateL rod) 7 3 (Shapes [])
     let piece = Translate (6,5) rod
     drawRegionInWindow uw Blue piece
     drawShapesInWindow uw Red board
     print $ hitBoard piece board
     spaceClose (win uw)
     
-- tests eliminating full rows
test6 = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Test 6 Window 1"
     let board = testBoard
     drawShapesInWindow uw Red board
     spaceClose (win uw)
     
test6b = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Test 6 Window 2"
     let board = testBoard
     let newBoard = deleteFullRows testBoard
     drawShapesInWindow uw Red newBoard
     spaceClose (win uw)
     
testBoarda = addToBoard upsideDown 8 2 (addToBoard upsideDown 5 2 (addToBoard upsideDown 2 2 (Shapes [])))
testBoardb = (addToBoard rod 3 3 (addToBoard (RotateL rod) 9 5 (addToBoard (RotateL rod) 10 3 testBoarda)))
testBoard = addToBoard rod 7 3 (addToBoard upsideDown 5 5 (addToBoard upsideDown 2 5 testBoardb))

upsideDown = RotateL (RotateL tPiece)
     

  
     
     
------------------------------------------------------------------------
-- Piece definitions

unitSq = Shape $ Block (0,0) 0.5 Blue
rect2 = unitSq `Union` Translate (1,0) unitSq
rect3 = Translate (-1,0) unitSq `Union` rect2

rod = Translate (-2,0) unitSq `Union` rect3
block = rect2 `Union` Translate (0,1) rect2
tPiece = rect3 `Union` (Translate (0,1) unitSq)
lPiece = rect3 `Union` (Translate (1,1) unitSq)
lPieceM = rect3 `Union` (Translate (-1,1) unitSq)
jog = rect2 `Union` (Translate (1,1) rect2)
jogM = rect2 `Union` (Translate (1,-1) rect2)

------------------------------------------------------------------------  
------------------------------------------------------------------------
-- Flatten blocks

-- Converts Region that is composed from Block shapes by rotation, translation, and union
-- to a Shapes region holding a list of transformed Blocks.
-- (Error for any other type of region.)
flattenBlocks :: Region -> Region
flattenBlocks = Shapes . (flhelp (0,0) 0)

flhelp :: Vector -> Int -> Region -> [Shape]

flhelp (lx,ly) rot (Translate (u,v) r) 
  = let (u1,v1) = rotL rot (u,v)
    in flhelp (lx+u1,ly+v1) rot r
  
flhelp loc rot (RotateL r)
  = flhelp loc ((rot+1) `mod` 4) r

flhelp loc rot (r1 `Union` r2)
  = flhelp loc rot r1 ++ flhelp loc rot r2

flhelp (lx,ly) rot (Shape (Block (cx,cy) r color))
  = let (x1,y1) = rotL rot (cx,cy)
    in [Block (lx+x1, ly+y1) r color]

-- Rotate a vector k times to the left 90 degrees
rotL        :: Int -> Vector -> Vector
rotL 0 v     = v
rotL k (x,y) = rotL (k-1) (-y, x) 

-- Special-purpose version of drawRegionInWindow
--drawShapesInWindow :: UserWindow -> Color -> Region -> IO ()
--drawShapesInWindow uw c (Shapes ss)
--  = mapM_ (\s -> drawInWindow (win uw) (withColor c (shapeToGraphic uw s))) ss
drawShapesInWindow :: UserWindow -> Region -> IO ()
drawShapesInWindow uw (Shapes ss)
  = mapM_ (\s -> drawInWindow (win uw) (withColor (getColor s) (shapeToGraphic uw s))) ss
  
getColor (Block (x,y) r color) = color


