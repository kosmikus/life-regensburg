
import Data.Set
import Test.QuickCheck

type X     = Integer
type Y     = Integer
type Loc   = (X, Y)
type World = Set Loc

isAlive :: World -> Loc -> Bool
isAlive = flip member

neighbors :: Loc -> [Loc]
neighbors (x, y) =
  [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
  , (x - 1, y    ),             (x + 1, y    )
  , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
  ]

neighbors' :: Loc -> [Loc]
neighbors' (x, y) = [ (x0, y0)
                    | x0 <- [x - 1 .. x + 1]
                    , y0 <- [y - 1 .. y + 1]
                    , (x0, y0) /= (x, y)
                    ]

prop_neighbors_neighbors' loc = fromList (neighbors loc) == fromList (neighbors' loc)

rule :: Bool -> Int -> Bool
rule True  alive = alive `elem` [2, 3]
rule False alive = alive == 3


