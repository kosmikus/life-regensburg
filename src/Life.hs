{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Char
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import Data.Void
import Graphics.Gloss
import System.Environment
import System.Exit
import Text.Megaparsec as P
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer
import Text.Read (readMaybe)

data Position = Pos !Int !Int
  deriving (Eq, Ord, Show)

newtype World = World (Map Position Age)
  deriving (Show)

type Age   = Int

(.+.) :: Position -> Position -> Position
Pos x1 y1 .+. Pos x2 y2 = Pos (x1 + x2) (y1 + y2)

isAlive :: World -> Position -> Bool
isAlive (World m) pos =
  M.member pos m

-- | Defaults age of dead position to 0.
currentAge :: World -> Position -> Int
currentAge (World m) pos =
  M.findWithDefault 0 pos m

neighbours :: Position -> Set Position
neighbours pos =
  S.map (pos .+.)
    (S.fromList
      [ Pos x y
      | x <- [-1 .. 1]
      , y <- [-1 .. 1]
      , not ((x == 0) && (y == 0))
      ]
    )

countLivingNeighbours :: World -> Position -> Int
countLivingNeighbours (World m) pos =
  M.size (M.restrictKeys m (neighbours pos))

rule :: Bool -> Int -> Bool
rule True  alive = alive `elem` [2, 3]
rule False alive = alive == 3

candidates :: World -> Set Position
candidates (World m) =
  let
    positionsAlive = M.keysSet m
  in
    S.union positionsAlive (S.unions (S.map neighbours positionsAlive))

step :: World -> World
step world =
  World
    (M.mapMaybeWithKey
      (\ loc age ->
        if rule (isAlive world loc) (countLivingNeighbours world loc)
          then Just (age + 1)
          else Nothing
      )
      (M.fromSet (currentAge world) (candidates world))
    )

-- | Create a world from a list of positions, initialising age to 1.
makeWorld :: [Position] -> World
makeWorld positions =
  World (M.fromList (L.map (\ pos -> (pos, 1)) positions))

glider :: World
glider =
  makeWorld
    [Pos 0 (-1), Pos 1 0, Pos (-1) 1, Pos 0 1, Pos 1 1]

renderBlock :: Position -> Age -> Picture
renderBlock (Pos x y) age =
  color
    (ageToColor age)
      (translate
        (fromIntegral x)
        (fromIntegral (-y))
        (rectangleSolid 0.8 0.8)
      )

-- | Map age to colour. White for new cells, black for
-- infinitely old cells. Getting darker over time.
--
ageToColor :: Age -> Color
ageToColor n = mixColors 3 (fromIntegral (n - 1)) white black

renderWorld :: World -> Picture
renderWorld (World m) =
  pictures (fmap (uncurry renderBlock) (M.toList m))

simulateWorld :: Int -> World -> IO ()
simulateWorld speed model =
  simulate
    FullScreen
    red
    speed -- rendering speed
    model
    (scale 10 10 . renderWorld) -- prescale by factor of 10
    (\ _viewport _time world -> step world)

-- | Takes two command line arguments, the name of the
-- rle file to read and the speed.
--
main :: IO ()
main = do
  args <- getArgs
  go args
  where
    go [file, rate] | Just speed <- readMaybe rate = do
      contents <- readFile file
      world <- readRle contents
      simulateWorld speed world
    go _ = do
       putStrLn "Usage: life [filename] [speed]"
       exitFailure

-- | Read a lif106 format file, which is yet a simpler
-- format than rle. Not used.
--
readLif106 :: String -> World
readLif106 contents =
    makeWorld
  . fmap (\ [x, y] -> Pos (read x) (read y))
  . fmap words
  . L.filter (\ l -> L.take 1 l /= "#")
  $ lines contents

data RLEFile =
  RLEFile
    Position -- dimensions (we do not use them)
    [RLEInstr]

data RLEInstr =
    RLEAlive !Int
  | RLEDead !Int
  | RLENewline !Int

readRle :: String -> IO World
readRle contents =
  case parse parseRle "input" contents of
    Left e  -> putStr (errorBundlePretty e) >> exitFailure
    Right w -> return (rleToWorld w)

rleToWorld :: RLEFile -> World
rleToWorld (RLEFile _dims instructions) =
  makeWorld (go (Pos 0 0) instructions)
  where
    go :: Position -> [RLEInstr] -> [Position]
    go _pos      []                      = []
    go (Pos _ y) (RLENewline n : instrs) = go (Pos 0 (y + n)) instrs
    go (Pos x y) (RLEDead    n : instrs) = go (Pos (x + n) y) instrs
    go (Pos x y) (RLEAlive   n : instrs) = positions ++ go (Pos (x + n) y) instrs
      where
        positions = [ Pos x' y | x' <- [x .. x + n - 1] ]

parseRle :: Parsec Void String RLEFile
parseRle =
  RLEFile <$ genspace <*> parseDimensions <*> parseInstrs

-- | Space but not newline.
sp1 :: Parsec Void String ()
sp1 =
  () <$ takeWhile1P (Just "space") (\ x -> isSpace x && x /= '\n')

genspace :: Parsec Void String ()
genspace =
  space
    space1
    (skipLineComment "#")
    P.empty

gentoken :: String -> Parsec Void String String
gentoken x =
  lexeme genspace (string x)

headerspace :: Parsec Void String ()
headerspace =
  space
    sp1
    P.empty
    P.empty

headertoken :: String -> Parsec Void String String
headertoken x =
  lexeme headerspace (string x)

restOfFirstLine :: Parsec Void String ()
restOfFirstLine =
  () <$ takeWhileP (Just "rest of line") (\ x -> x /= '\n')

-- Parse the first line
parseDimensions :: Parsec Void String Position
parseDimensions =
  Pos
    <$ headertoken "x" <* headertoken "=" <*> parseInt <* headertoken ","
    <* headertoken "y" <* headertoken "=" <*> parseInt <* restOfFirstLine <* newline <* genspace

parseInt :: Parsec Void String Int
parseInt =
  lexeme headerspace decimal

parseInstrs :: Parsec Void String [RLEInstr]
parseInstrs =
  many parseInstr <* gentoken "!"

parseInstr :: Parsec Void String RLEInstr
parseInstr =
  flip ($) <$> option 1 decimal <*>
    (   RLEAlive   <$ gentoken "o"
    <|> RLEDead    <$ gentoken "b"
    <|> RLENewline <$ gentoken "$"
    )

renderGlider :: IO ()
renderGlider =
  display
    FullScreen
    red
    (scale 10 10 $ renderWorld glider)
