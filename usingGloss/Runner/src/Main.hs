module Main where



import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Color
import System.Random

type Height = Int
type Width = Int
type PosX = Float
type PosY = Float

data Cell = Cell PosX PosY Color

data World = World Height Width PosX PosY [Cell] StdGen

data Direction = UP | DOWN | LEFT | RIGHT


delta :: Float
delta = 0.001 --for float comparison (when we check if the runner has hit a boundary)

main :: IO ()
main = do
  gen <- newStdGen
  let (startX , tmpGen) = randomR (0,1000) gen
      (startY, startGen) = randomR (0,700) tmpGen
  simulate (InWindow "Runner" (1000, 700) (10,10))  black 30 (World 1000 700 startX startY [] startGen) drawFun viewPortFun --yes animate would be better/sufficient but I wanted to test the simulate function



drawFun :: World -> Picture
drawFun (World _ _ _ _ cs _) = pictures $ map cellToPicture cs
  where
    cellToPicture :: Cell -> Picture
    cellToPicture (Cell px py col) = color col  $ translate px py $ rectangleSolid 1 1

viewPortFun :: ViewPort -> Float -> World -> World
viewPortFun _ _ (World h w px py cs g) = World h w newX newY (Cell newX newY col : cs) newGen
  where
    (col,tmpGen) = getColor g
    (dir,newGen) = getDirection tmpGen
    (newX,newY) = getNewPos dir px py h w


getNewPos :: Direction -> PosX -> PosY -> Height -> Width -> (PosX,PosY)
getNewPos dir px py h w=
  case dir of
    UP     -> (px , py + if py - 1.0  + delta < 0 then 1  else (-1) )
    DOWN   -> (px , py + if py + 1.0 - delta > fromIntegral h then (-1) else 1 )
    LEFT   -> (px + if px - 1.0 + delta < 0 then 1 else (-1), py)
    RIGHT  -> (px + if px + 1.0 + delta > fromIntegral w then (-1) else 1 , py)

getColor :: StdGen -> (Color,StdGen)
getColor g  = (colorArr !! choosenColor, newG)
  where
    (choosenColor,newG) = randomR (0,length colorArr - 1) g
    colorArr = [yellow,orange,violet,green,azure,aquamarine,rose]

getDirection :: StdGen -> (Direction,StdGen)
getDirection g = (dirArr !! choosenDir , newG)
  where
    (choosenDir,newG) = randomR (0,3) g
    dirArr = [UP,DOWN,LEFT,RIGHT]
