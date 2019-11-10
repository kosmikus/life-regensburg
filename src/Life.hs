{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.List as L
import Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Environment

type X     = Integer
type Y     = Integer
type Loc   = (X, Y)
type World = Set Loc

isAlive :: World -> Loc -> Bool
isAlive = flip member

neighbors :: Loc -> Set Loc
neighbors (x, y) =
  fromList
    [ (x0, y0)
    | x0 <- [x - 1 .. x + 1]
    , y0 <- [y - 1 .. y + 1]
    ]

countLivingNeighbors :: World -> Loc -> Int
countLivingNeighbors world loc =
  size (intersection world (neighbors loc))

rule :: Bool -> Int -> Bool
rule True  alive = alive `elem` [3, 4]
rule False alive = alive == 3

candidates :: World -> Set Loc
candidates world =
  unions (fmap neighbors (toList world))

step :: World -> World
step world =
  S.filter
    (\ loc -> rule (isAlive world loc) (countLivingNeighbors world loc))
    (candidates world)

glider :: World
glider =
  fromList [(0, -1), (1, 0), (-1, 1), (0, 1), (1, 1)]

renderBlock :: Loc -> Picture
renderBlock (x, y) =
  Color
    white
      (Translate
        (fromIntegral x)
        (fromIntegral (-y))
        (Polygon [(-0.4,-0.4), (-0.4, 0.4), (0.4, 0.4), (0.4, -0.4)])
      )

renderWorld :: World -> Picture
renderWorld world =
  Pictures (fmap renderBlock (toList world))

simulateWorld :: Int -> World -> IO ()
simulateWorld speed model =
  simulate
    (FullScreen) -- (InWindow "Game of Life" (100, 100) (100, 100))
    red
    speed
    model
    (applyViewPortToPicture (ViewPort (0,0) 0 10) . renderWorld)
    (\ _viewport _time world -> step world)

main :: IO ()
main = do
  [file, rate] <- getArgs
  world <- fmap readLif106 (readFile file)
  simulateWorld (read rate) world

readLif106 :: String -> World
readLif106 contents =
    fromList
  . fmap (\ [x, y] -> (read x, read y))
  . fmap words
  . L.filter (\ l -> L.take 1 l /= "#")
  $ lines contents

renderGlider :: IO ()
renderGlider =
  display
    (InWindow "Glider" (100, 100) (100, 100))
    black
    (renderWorld glider)
