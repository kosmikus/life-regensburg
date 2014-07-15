
import Data.Set as S

type X     = Integer
type Y     = Integer
type Loc   = (X, Y)
type World = Set Loc

isAlive :: World -> Loc -> Bool
isAlive = flip member

neighbors :: Loc -> Set Loc
neighbors (x, y) = fromList
                     [ (x0, y0)
                     | x0 <- [x - 1 .. x + 1]
                     , y0 <- [y - 1 .. y + 1]
                     ]

countLivingNeighbors :: World -> Loc -> Int
countLivingNeighbors world loc = size (intersection world (neighbors loc))

rule :: Bool -> Int -> Bool
rule True  alive = alive `elem` [3, 4]
rule False alive = alive == 3

candidates :: World -> Set Loc
candidates world = unions (fmap neighbors (toList world))

step :: World -> World
step world = S.filter (\ loc -> rule (isAlive world loc) (countLivingNeighbors world loc))
                      (candidates world)
