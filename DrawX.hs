-- A modified version of Draw from SOE that:
-- 1) Allows arbitrary x and y ranges for the user coordinates 
-- 2) Draws Polyline shapes, defined in the expanded Shapes.hs
-- by Scot Drysdale 10/07
-- 3) Handles Block Shape
-- by Chris Bailey-Kellogg winter 2010

module DrawX ( UserWindow(WinData, win, userXmin, userXrange, 
              userYmax, userYrange, xWin, yWin), ColoredShapes,
              intToFloat, openUserWindow, 
              userXtoWin, userYtoWin, winXtoUser, winYtoUser,
              trans, iTrans, transList, iTransList,
              shapeToGraphic, spaceClose, drawShapes,
              module ShapeX,
            ) where

import ShapeX    -- Must be the updated version of shape
import SOE

type ColoredShapes = [(Color,Shape)]

-- Holds the information needed to plot in user coordinates
data UserWindow = WinData {win :: Window, 
                           userXmin :: Float, userXrange :: Float,
                           userYmax :: Float, userYrange :: Float, 
                           xWin :: Int, yWin :: Int}

-- Opens a graphics window and stores information about user range and window
-- range in a UserWindow data structure, which is returned.
openUserWindow :: Float -> Float -> Float -> Float -> Int -> Int -> 
                  String -> IO UserWindow
openUserWindow uXmin uXmax uYmin uYmax winX winY title =
  do
    w <- openWindow title (winX, winY)
    return (WinData w uXmin (uXmax - uXmin) uYmax (uYmax - uYmin) winX winY)
                  
userXtoWin     :: UserWindow -> Float -> Int
userXtoWin uw x  = round (intToFloat (xWin uw) * (x - userXmin uw)/userXrange uw)

userYtoWin     :: UserWindow -> Float -> Int
userYtoWin uw y = round (intToFloat (yWin uw) * (userYmax uw - y)/userYrange uw)

winXtoUser     :: UserWindow -> Int -> Float
winXtoUser uw x = userXrange uw * intToFloat x / intToFloat (xWin uw) + userXmin uw

winYtoUser     :: UserWindow -> Int -> Float
winYtoUser uw y = userYmax uw - userYrange uw * intToFloat y / intToFloat (yWin uw)

intToFloat  n = fromInteger (toInteger n)

-- Translate a single vertex from user coordinates to window coordinates
trans :: UserWindow -> Vertex -> Point
trans uw (x,y)   = ( userXtoWin uw x, userYtoWin uw y )

-- Translate a single point from window coordinates to user coordinates
-- (Inverse of trans.)
iTrans :: UserWindow -> Point -> Vertex
iTrans uw (x,y)   = ( winXtoUser uw x, winYtoUser uw y )

-- Translate a list of vertices from user coordinates to window coordinates
transList :: UserWindow -> [Vertex] -> [Point]
transList uw = map (trans uw)

-- Translate a list of points from window coordinates to user coordinates
-- (Inverse of transList.)
iTransList :: UserWindow -> [Point] -> [Vertex]
iTransList uw = map (iTrans uw)

-- Convert a Shape type into an equivalent Graphic type
shapeToGraphic :: UserWindow -> Shape -> Graphic
shapeToGraphic uw (Block (cx,cy) r color)
    = polygon (transList uw [(cx-r,cy-r),(cx-r,cy+r),(cx+r,cy+r),(cx+r,cy-r)])
shapeToGraphic uw (Rectangle s1 s2)
  = let s12 = s1/2
        s22 = s2/2
    in polygon 
         (transList uw [(-s12,-s22),(-s12,s22),
                        (s12,s22),(s12,-s22)])
shapeToGraphic uw (Ellipse r1 r2)
  = ellipse (trans uw (-r1,-r2)) (trans uw (r1,r2)) 
shapeToGraphic uw (RtTriangle s1 s2)
  = polygon (transList uw [(0,0),(s1,0),(0,s2)])
shapeToGraphic uw (Polygon pts)
  = polygon (transList uw pts)
shapeToGraphic uw (Polyline pts)
  = polyline (transList uw pts)

------- The following code is for testing and demostrations

sh1,sh2,sh3,sh4, sh5 :: Shape

sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5,2.5), (-1.5,2.0), (-1.1,0.2),
               (-1.7,-1.0), (-3.0,0)]
sh5 = Polyline [(-2.5, -2.5), (-1, 0), (1, 0), (2.5, 2.5)]

main0
  = runGraphics (
    do uw <- openUserWindow (-3) 3 (-3) 3 600 600 "Drawing Shapes" 
       let w = win uw
       drawInWindow w (withColor Red  (shapeToGraphic uw sh1))
       drawInWindow w (withColor Blue (shapeToGraphic uw sh2))
       drawInWindow w (withColor Cyan (shapeToGraphic uw sh5))
       spaceClose w 
    )

shs :: ColoredShapes
shs  = [(Red,sh1),(Blue,sh2),(Yellow,sh3),(Magenta,sh4), (Cyan, sh5)]

drawShapes :: UserWindow -> ColoredShapes -> IO ()
drawShapes uw [] 
  = return ()
drawShapes uw ((c,s):cs)
  = do drawInWindow (win uw) (withColor c (shapeToGraphic uw s))
       drawShapes uw cs

main1
  = runGraphics (
    do uw <- openUserWindow (-3) 3 (-3) 3 600 600 "Drawing Shapes" 
       drawShapes uw shs
       spaceClose (win uw)
    )


spaceClose :: Window -> IO ()
spaceClose w
  = do k <- getKey w
       if k==' ' || k == '\x0'
          then closeWindow w
          else spaceClose w

main2book
  = runGraphics (
    do uw <- openUserWindow (-2.5) 2.5 (-2.5) 2.5 400 400 "Bull's Eye"
       let w = win uw
       drawInWindow w (withColor White
         (polygon [(0,0),(xWin uw,0),(xWin uw ,yWin uw),(0,yWin uw)]))
       drawShapes uw coloredCircles
       spaceClose w
    )

conCircles = map circle [2.4,2.1 .. 0.3] -- [1.6,1.4 .. 0.2]

coloredCircles = 
  zip [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White]
      conCircles


