{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Char
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Void
import Graphics.Gloss
import System.Environment
import Text.Megaparsec as P
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer

type X     = Int
type Y     = Int
type Loc   = (X, Y)
type Age   = Int
type World = Map Loc Age

isAlive :: World -> Loc -> Bool
isAlive = flip M.member

neighbors :: Loc -> Set Loc
neighbors (x, y) =
  S.fromList
    [ (x0, y0)
    | x0 <- [x - 1 .. x + 1]
    , y0 <- [y - 1 .. y + 1]
    ]

countLivingNeighbors :: World -> Loc -> Int
countLivingNeighbors world loc =
  M.size (M.restrictKeys world (neighbors loc))

rule :: Bool -> Int -> Bool
rule True  alive = alive `elem` [3, 4]
rule False alive = alive == 3

candidates :: World -> Set Loc
candidates world =
  S.unions (fmap neighbors (M.keys world))

step :: World -> World
step world =
  M.mapMaybeWithKey
    (\ loc age -> if rule (isAlive world loc) (countLivingNeighbors world loc) then Just (age + 1) else Nothing)
    (M.fromSet (\ loc -> M.findWithDefault 0 loc world) (candidates world))

glider :: World
glider =
    M.fromSet (const 1)
  $ S.fromList [(0, -1), (1, 0), (-1, 1), (0, 1), (1, 1)]

renderBlock :: Loc -> Age -> Picture
renderBlock (x, y) age =
  Color
    (ageToColor age)
      (translate
        (fromIntegral x)
        (fromIntegral (-y))
        (rectangleSolid 0.8 0.8)
        -- (Polygon [(-0.4,-0.4), (-0.4, 0.4), (0.4, 0.4), (0.4, -0.4)])
      )

ageToColor :: Age -> Color
ageToColor 0 = black
ageToColor 1 = white
ageToColor n = mixColors 3 (fromIntegral n) white black

renderWorld :: World -> Picture
renderWorld world =
  Pictures (fmap (uncurry renderBlock) (M.toList world))

simulateWorld :: Int -> World -> IO ()
simulateWorld speed model =
  simulate
    FullScreen
    red
    speed -- rendering speed
    model
    (scale 10 10 . renderWorld) -- prescale by factor of 10
    (\ _viewport _time world -> step world)

main :: IO ()
main = do
  [file, rate] <- getArgs
  world <- fmap readRle (readFile file)
  simulateWorld (read rate) world

readLif106 :: String -> World
readLif106 contents =
    M.fromSet (const 1)
  . S.fromList
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
  M.fromSet (const 1) (go (0, 0) S.empty entries)
  where
    go :: Loc -> Set Loc -> [RLEInstr] -> Set Loc
    go (!_x, !_y) !world [] = world
    go (!_x, !y) !world (RLENewline n : instrs) = go (0, y + n) world instrs
    go (!x, !y) !world (RLEDead n : instrs) = go (x + n, y) world instrs
    go (!x, !y) !world (RLEAlive n : instrs) = go (x + n, y) world' instrs
      where
        world' = S.union world (S.fromList [(x', y) | x' <- [x .. x + n - 1]])

parseRle :: Parsec Void String RLEFormat
parseRle = do
  genspace
  (x, y) <- parseDimensions
  instrs <- parseInstrs
  return (RLEFormat (x, y) instrs)

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

-- | Space including newline

parseDimensions :: Parsec Void String (Int, Int)
parseDimensions =
  (,)
    <$ headertoken "x" <* headertoken "=" <*> parseInt <* headertoken ","
    <* headertoken "y" <* headertoken "=" <*> parseInt <* newline <* genspace

parseInt :: Parsec Void String Int
parseInt =
  lexeme headerspace decimal

parseInstrs :: Parsec Void String [RLEInstr]
parseInstrs =
  many parseInstr <* gentoken "!"

parseInstr :: Parsec Void String RLEInstr
parseInstr =
  flip ($) <$> option 1 decimal <*> (RLEAlive <$ gentoken "o" <|> RLEDead <$ gentoken "b" <|> RLENewline <$ gentoken "$")

renderGlider :: IO ()
renderGlider =
  display
    FullScreen
    red
    (scale 10 10 $ renderWorld glider)
