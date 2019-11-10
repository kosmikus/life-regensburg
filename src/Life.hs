{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.List as L
-- import Data.Map as M
import Data.Set as S
import Data.Void
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char

type X     = Int
type Y     = Int
type Loc   = (X, Y)
type World = Set Loc

isAlive :: World -> Loc -> Bool
isAlive = flip S.member

neighbors :: Loc -> Set Loc
neighbors (x, y) =
  S.fromList
    [ (x0, y0)
    | x0 <- [x - 1 .. x + 1]
    , y0 <- [y - 1 .. y + 1]
    ]

countLivingNeighbors :: World -> Loc -> Int
countLivingNeighbors world loc =
  S.size (S.intersection world (neighbors loc))

rule :: Bool -> Int -> Bool
rule True  alive = alive `elem` [3, 4]
rule False alive = alive == 3

candidates :: World -> Set Loc
candidates world =
  S.unions (fmap neighbors (S.toList world))

step :: World -> World
step world =
  S.filter
    (\ loc -> rule (isAlive world loc) (countLivingNeighbors world loc))
    (candidates world)

glider :: World
glider =
  S.fromList [(0, -1), (1, 0), (-1, 1), (0, 1), (1, 1)]

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
  Pictures (fmap renderBlock (S.toList world))

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
  world <- fmap readRle (readFile file)
  simulateWorld (read rate) world

readLif106 :: String -> World
readLif106 contents =
    S.fromList
  . fmap (\ [x, y] -> (read x, read y))
  . fmap words
  . L.filter (\ l -> L.take 1 l /= "#")
  $ lines contents

data RLEFormat =
  RLEFormat
    Loc -- dimensions
    [RLEInstr]

data RLEInstr =
    RLEAlive !Int
  | RLEDead !Int
  | RLENewline !Int

readRle :: String -> World
readRle contents =
  case parse parseRle "input" contents of
    Left e -> error (errorBundlePretty e)
    Right w  -> interpretRLEFormat w

interpretRLEFormat :: RLEFormat -> World
interpretRLEFormat (RLEFormat _ entries) =
  go (0, 0) S.empty entries
  where
    go :: Loc -> World -> [RLEInstr] -> World
    go (!_x, !_y) !world [] = world
    go (!_x, !y) !world (RLENewline n : instrs) = go (0, y + n) world instrs
    go (!x, !y) !world (RLEDead n : instrs) = go (x + n, y) world instrs
    go (!x, !y) !world (RLEAlive n : instrs) = go (x + n, y) world' instrs
      where
        world' = S.union world (S.fromList [(x', y) | x' <- [x .. x + n - 1]])

parseRle :: Parsec Void String RLEFormat
parseRle = do
  (x, y) <- parseDimensions
  instrs <- parseInstrs
  return (RLEFormat (x, y) instrs)

parseDimensions :: Parsec Void String (Int, Int)
parseDimensions =
  (,)
    <$ string "x" <* space <* string "=" <* space <*> parseInt <* space <* string "," <* space
    <* string "y" <* space <* string "=" <* space <*> parseInt <* newline

parseInt :: Parsec Void String Int
parseInt =
  read <$> some digitChar

parseInstrs :: Parsec Void String [RLEInstr]
parseInstrs =
  many parseInstr <* (string "!") <* space

parseInstr :: Parsec Void String RLEInstr
parseInstr =
  flip ($) <$> option 1 parseInt <*> (RLEAlive <$ string "o" <|> RLEDead <$ string "b" <|> RLENewline <$ string "$") <* space

renderGlider :: IO ()
renderGlider =
  display
    (InWindow "Glider" (100, 100) (100, 100))
    black
    (renderWorld glider)
