-- Tetris pieces: creation, containment tests, and display
-- Using original SOE data types
-- Chris Bailey-Kellogg, winter 2010, based on Scot Drysdale's tetris code



import SOE hiding (Region)
import qualified SOE as G (Region)

import DrawX
import ShapeX
import RegionEX
import PictureEX



------------------------------------------------------------------------
-- Piece definitions

unitSq = Shape $ Rectangle 1 1
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
-- Draw transformed versions and test point containment

columns = 10                         -- Columns in the board
rows = 15                            -- Number of rows in the board
squareSize = 30                      -- Square side length in pixels
winX = round (columns * squareSize)  -- Width of drawing window in pixels
winY = round (rows * squareSize)     -- Height of drawing window in pixels

openUserWindowEx uXmin uXmax uYmin uYmax winX winY title = 
  do w <- openWindowEx title (Just (0,0)) (Just (winX,winY)) drawBufferedGraphic
     return (WinData w uXmin (uXmax - uXmin) uYmax (uYmax - uYmin) winX winY)

main = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Pieces 1"
     let pieces' = [Translate (5,6) $ RotateL rod,
                    Translate (2,13) block,
                    Translate (8,3) tPiece,
                    Translate (9,9) $ RotateL lPiece,
                    Translate (4,1) lPieceM,
                    Translate (6,12) $ RotateL jog,
                    Translate (1,9) jogM]
     mapM_ (\(p,c) -> drawRegionInWindow uw c p) $ 
           zip pieces' [Blue, Green, Cyan, Red, Magenta, Yellow, White]
     mapM_ (\(p,c) -> print $ p `containsR` c) $
           zip pieces' [(5,6),(2,13),(8,3),(9,9),(4,1),(6,12),(1,9)]
     mapM_ (\p -> print $ p `containsR` (1,1)) pieces'
     spaceClose (win uw)
     
main2 = runGraphics $
  do uw <- openUserWindowEx 0.5 (columns + 0.5) 0.5 (rows + 0.5) winX winY "Pieces 1"
     let pieces' = [Translate (1,2) $ RotateL jogM]
     mapM_ (\(p,c) -> drawRegionInWindow uw c p) $ 
           zip pieces' [Blue]
     spaceClose (win uw)
