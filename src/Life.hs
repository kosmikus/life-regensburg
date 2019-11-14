{-# OPTIONS_GHC -Wall #-}

import qualified Data.Set as S
import Graphics.Gloss
import Text.Megaparsec as P hiding (count)
import Text.Megaparsec.Char as P
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void
import System.Environment

data Position =
  Pos Int Int
  deriving (Eq, Ord, Show)

(.+.) :: Position -> Position -> Position
Pos x1 y1 .+. Pos x2 y2 = Pos (x1 + x2) (y1 + y2)

data World =
  World (S.Set Position)
  deriving Show

triple :: World
triple =
  World
    (S.fromList [Pos 0 (-1), Pos 0 0, Pos 0 1])

r :: World
r =
  World
    (S.fromList [Pos 0 0, Pos 1 0, Pos 0 1])

glider :: World
glider =
  World
    (S.fromList
      [ Pos 0 (-1), Pos 1 0, Pos (-1) 1, Pos 0 1, Pos 1 1 ]
    )

renderWorld :: World -> Picture
renderWorld (World set) =
  scale 10 10
    (mconcat (map renderPosition (S.toList set)))

renderPosition :: Position -> Picture
renderPosition (Pos x y) =
  color white
    (translate (fromIntegral x) (fromIntegral (-y))
      (rectangleSolid 0.8 0.8)
    )

step :: (Bool -> Int -> Bool) -> World -> World
step ruleFunction world =
  World
    (S.filter
      (\ pos ->
        ruleFunction
          (isAlive world pos)
          (nbsAlive world pos)
      )
      (candidates world)
    )

isAlive :: World -> Position -> Bool
isAlive (World set) pos = S.member pos set

nbs :: Position -> S.Set Position
nbs pos =
  S.map (.+. pos)
    (S.fromList
      [ Pos (-1) (-1), Pos 0 (-1), Pos 1 (-1)
      , Pos (-1)   0 ,             Pos 1   0
      , Pos (-1)   1 , Pos 0   1 , Pos 1   1
      ]
    )

nbsAlive :: World -> Position -> Int
nbsAlive world pos =
  length (filter (isAlive world) (S.toList (nbs pos)))

rule :: Bool -> Int -> Bool
rule False count = count == 3
rule True  count = count `elem` [2,3]

candidates :: World -> S.Set Position
candidates (World set) =
  set `S.union` S.unions (S.map nbs set)

data RLEFile =
  RLEFile [RLEInstr]
  deriving (Show)

data RLEInstr =
  Alive Int | Dead Int | Newline Int
  deriving (Show)

toWorld :: RLEFile -> World
toWorld (RLEFile instrs) = go (Pos 0 0) (World S.empty) instrs
  where
    -- takes current position and current world as arguments
    go :: Position -> World -> [RLEInstr] -> World
    go _currentPos world [] = world
    go (Pos x y) (World set) (Alive n : is) =
      go (Pos (x + n) y) (World (set `S.union` S.fromList [ Pos x' y | x' <- [x .. x + n - 1] ])) is
    go currentPos world (Dead n : is) =
      go (currentPos .+. Pos n 0) world is
    go (Pos _x y) (World set) (Newline n : is) =
      go (Pos 0 (y + n)) (World set) is

rleFile :: Parsec Void String RLEFile
rleFile =
  (\ instrs _ -> RLEFile instrs) <$> many rleInstr <*> string "!"

rleInstr :: Parsec Void String RLEInstr
rleInstr =
  (\ count instrf _ -> instrf count) <$> option 1 decimal <*> rleInstr' <*> space

rleInstr' :: Parsec Void String (Int -> RLEInstr)
rleInstr' =
      (\ _ -> Dead   ) <$> string "b"
  <|> (\ _ -> Alive  ) <$> string "o"
  <|> (\ _ -> Newline) <$> string "$"

oldMain :: IO ()
oldMain =
  -- display FullScreen red (renderWorld r)
  simulate
    FullScreen
    red
    20
    glider
    renderWorld
    (\ _ _ -> step rule)

-- | Expects filename as argument.
main :: IO ()
main = do
  [file] <- getArgs
  contents <- readFile file
  case parse rleFile "" contents of
    Left e -> print e
    Right rle ->
      simulate
        FullScreen
        red
        20
        (toWorld rle)
        renderWorld
        (\ _ _ -> step rule)
