-- Modified version of Picture.lhs from SOE.  Allows user coordinates
-- to be specified for the window and 90 degree rotations.
-- by Scot Drysdale on 11/8/07

module PictureEX ( Picture (Region, Over, EmptyPic),
                 Color (Black, Blue, Green, Cyan, 
                        Red, Magenta, Yellow, White),
                 regionToGRegion, shapeToGRegion, winRect,
                 drawRegionInWindow, drawPic, draw, spaceClose,
                 module RegionEX
                ) where
                
import DrawX
import RegionEX
import SOE hiding (Region)
import qualified SOE as G (Region)

data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
     deriving Show


-- Draws a Picture onto the user-coordinates window uw.
drawPic                 :: UserWindow -> Picture -> IO ()
drawPic uw (Region c r)   = drawRegionInWindow uw c r
drawPic uw (p1 `Over` p2) = do drawPic uw p2; drawPic uw p1
drawPic uw EmptyPic       = return ()

-- Draws Region r colored c onto the user-coordinates window uw
drawRegionInWindow :: UserWindow -> Color -> Region -> IO ()
drawRegionInWindow uw c r 
  = drawInWindow (win uw) 
      (withColor c 
         (drawRegion (regionToGRegion uw r)))

-- Converts a Region to a G.Region.  
-- User window uw is needed by regToGReg, so is passed.
regionToGRegion :: UserWindow -> Region -> G.Region
regionToGRegion uw r = regToGReg uw (0,0) (1,1) 0 r


-- Converts Regions to G.Regions using three accumulators to accumulate
-- the total translation, total scaling, and total rotation.  
-- User window uw needed for Complement and shapeToGRegion.

regToGReg :: UserWindow -> Vector -> Vector -> Int -> Region -> G.Region
regToGReg uw loc sca rot (Shape s) 
  = shapeToGRegion uw loc sca rot s

regToGReg uw loc (sx,sy) rot (Scale (u,v) r)
  = let (u1,v1) = if rot `mod` 2 == 0 then (u,v) else (v,u)
    in regToGReg uw loc (sx*u1,sy*v1) rot r

regToGReg uw (lx,ly) (sx,sy) rot (Translate (u,v) r) 
  = let (u1,v1) = rotL rot (u,v)
    in regToGReg uw (lx+u1*sx,ly+v1*sy) (sx,sy) rot r
  
regToGReg uw (lx,ly) (sx,sy) rot (RotateL r)
  = regToGReg uw (lx,ly) (sx,sy) ((rot+1) `mod` 4) r

regToGReg uw loc sca rot Empty
  = createRectangle (0,0) (0,0)


regToGReg uw loc sca rot (r1 `Union` r2)
  = primGReg uw loc sca rot r1 r2 orRegion


regToGReg uw loc sca rot (r1 `Intersect` r2)
  = primGReg uw loc sca rot r1 r2 andRegion

regToGReg uw loc sca rot (r1 `Xor` r2)
  = primGReg uw loc sca rot r1 r2 xorRegion

regToGReg uw loc sca rot (Complement  r)
  = primGReg uw loc sca rot (winRect uw) r diffRegion


-- Helper function used by Union, Intersect, Xor, and Complement.
primGReg :: UserWindow -> Vector -> Vector -> Int -> Region -> Region -> 
            (G.Region -> G.Region -> G.Region) -> G.Region
primGReg uw loc sca rot r1 r2 op
  = let gr1 = regToGReg uw loc sca rot r1
        gr2 = regToGReg uw loc sca rot r2
    in  op gr1 gr2

-- Rotate a vector k times to the left 90 degrees
rotL        :: Int -> Vector -> Vector
rotL 0 v     = v
rotL k (x,y) = rotL (k-1) (-y, x) 

-- Get rectangle that covers screen.  Note that you have to translate the
-- origin to the center of the window, because a Rectangle is defined centered
-- at the origin.
winRect   :: UserWindow -> Region
winRect uw = Translate (userXmin uw + (userXrange uw)/2 , userYmax uw - (userYrange uw)/2) 
                       (Shape (Rectangle (userXrange uw) (userYrange uw)))


-- Converts s into a G.Region after scaling by (sx,sy) and translating by (lx,ly) and
-- rotating 90 degrees left rot times.
-- Note that the translations all convert User coordinates (from uw) into Window coordinates.
shapeToGRegion :: UserWindow -> Vector -> Vector -> Int -> Shape -> G.Region
shapeToGRegion uw (lx,ly) (sx,sy) rot s
  = case s of
      Block (cx,cy) r color
        -> createRectangle (trans (cx-r,cy-r))
                           (trans (cx+r,cy+r))
      Rectangle s1 s2  
        -> createRectangle (trans (-s1/2,-s2/2)) 
                           (trans (s1/2,s2/2))
      Ellipse r1 r2    
        -> createEllipse (trans (-r1,-r2))
                         (trans ( r1, r2))
      Polygon vs      
        -> createPolygon (map trans vs)
      RtTriangle s1 s2 
        -> createPolygon (map trans [(0,0),(s1,0),(0,s2)])
    where trans :: Vertex -> Point
          trans (x,y) = let (x1, y1) = rotL rot (x, y)
                        in ( userXtoWin uw (lx+x1*sx), 
                             userYtoWin uw (ly+y1*sy) )
          
          

-- Opens a user window with given dimensions and draws Picture p onto it.
draw :: Float -> Float -> Float -> Float -> Int -> Int -> String -> Picture -> IO ()
draw uXmin uXmax uYmin uYmax xMin xMax s p
  = runGraphics $
    do uw <- openUserWindow uXmin uXmax uYmin uYmax xMin xMax s
       drawPic uw p
       spaceClose (win uw)


-- An additonal operation on regions, sort of like Xor.
xUnion :: Region -> Region -> Region
p1 `xUnion` p2 = (p1 `Intersect` Complement p2) `Union`
                 (p2 `Intersect` Complement p1)

r1 = Shape (Rectangle 3 2)
r2 = Shape (Ellipse 1 1.5)
r3 = Shape (RtTriangle 3 2)
r4 = Shape (Polygon [(-2.5,2.5), (-3.0,0), (-1.7,-1.0),
                     (-1.1,0.2), (-1.5,2.0)])

reg1 = r3 `xUnion` (r1 `Intersect` Complement r2 `Union` r4)
pic1 = Region Blue reg1

reg2 = let circle = Shape (Ellipse 0.5 0.5)
           square = Shape (Rectangle 1 1)
       in (Scale (1,2) circle)
          `Union` (Translate (1,0) square)
          `Union` (Translate (-1,0) square)
pic2 = Region Yellow (Translate (0,-1) reg2)

pic3 = pic2 `Over` pic1

draw3 = draw (-4) 3 (-3) 4 300 300 "pic3" pic3

pic1R = Region Blue (RotateL reg1)
pic2R = Region Yellow (RotateL (Translate (0,-1) reg2))
pic3R = pic2R `Over` pic1R

draw3R = draw (-4) 3 (-3) 4 300 300 "pic3R" pic3R

drawU = draw (-4) 3 (-3) 4 300 300

r = Shape (Rectangle 1 2)
pp1 = Region Red (Translate (1,0) (Scale (0.5,1) r))
pp2 = Region Blue (Scale (0.5,1) (Translate (1,0) r))

pp1R = Region Red (RotateL (Translate (2,0) (Scale (0.5,1) r)))
pp2R = Region Blue (RotateL (Scale (0.5,1) (Translate (2,0) r)))


oneCircle   = Shape (Ellipse 1 1)
manyCircles = [ Translate (x,0) oneCircle | x <- [0,2..] ]
fiveCircles = foldr Union Empty (take 5 manyCircles)
fc = Region Red (Scale (0.25,0.25) fiveCircles)

r5 = let c1 = Shape (Ellipse 0.5 0.5)
         c2 = Translate (1,0) c1
         cs = Translate (1,1) (c1 `Union` c2)
     in Scale (0.5,0.5) cs

r6 = let c = Shape (Ellipse 0.5 0.5)
         s = Shape (Rectangle 1 1)
     in (Scale (2,2) c) `Union`
        ((Translate (2,0) s) `Union` 
         (Translate (-2,0) s))


-- Convert a Picture to a list of (color, region) pairs
pictToList :: Picture -> [(Color,Region)]
pictToList  EmptyPic      = []
pictToList (Region c r)   = [(c,r)]
pictToList (p1 `Over` p2) = pictToList p1 ++ pictToList p2


-- Find thing clicked on and pull out of list.
adjust :: [(Color,Region)] -> Coordinate -> 
          (Maybe (Color,Region), [(Color,Region)])
adjust regs p
  = case (break (\(_,r) -> r `containsR` p) regs) of
      (top,hit:rest) -> (Just hit, top++rest)
      (_,[])         -> (Nothing, regs)


-- Helper function for draw2 below
loop :: UserWindow -> [(Color,Region)] -> IO ()
loop uw regs = 
    do clearWindow (win uw)
       sequence_ [ drawRegionInWindow uw c r | (c,r) <- reverse regs ]
       (x,y) <- getLBP (win uw)
       case (adjust regs (winXtoUser uw x, 
                          winYtoUser uw y) )of
         (Nothing,  _      ) -> closeWindow (win uw)
         (Just hit, newRegs) -> loop uw (hit : newRegs)

-- Draws a set of regions, bringing the one clicked on to the front. 
draw2 :: Float -> Float -> Float -> Float -> Int -> Int -> String -> Picture -> IO ()
draw2 uXmin uXmax uYmin uYmax xMin xMax s p
  = runGraphics $
    do uw <- openUserWindow uXmin uXmax uYmin uYmax xMin xMax s
       loop uw (pictToList p)

p1,p2,p3,p4 :: Picture
p1 = Region Red (RotateL r1)
p2 = Region Blue (RotateL r2)
p3 = Region Green (RotateL r3)
p4 = Region Yellow (RotateL r4)

pic :: Picture 
pic = foldl Over EmptyPic [p1,p2,p3,p4]
main = draw2 (-4) 3 (-3) 4 300 300 "Picture Click Test" pic


