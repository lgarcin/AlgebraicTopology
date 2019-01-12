module Classification where

import qualified Data.Map as Map

data Orientation
  = Pos
  | Neg
  deriving (Show)

instance Eq Orientation where
  (==) Pos Pos = True
  (==) Neg Neg = True
  (==) _ _ = False

(-) :: Orientation -> Orientation
(-) Pos = Neg
(-) Neg = Pos

data Edge = Edge
  { edge_id :: Integer
  , orientation :: Orientation
  } deriving (Show)

inverse :: Edge -> Edge
inverse (Edge a o) = Edge a (-o)

type Polygon = [Edge]

type Surface = [Polygon]

occurrences :: (Ord a) => [a] -> [(a, Int)]
occurrences xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

admissible :: Surface -> Bool
admissible surf =
  all (\x -> x == 2) ((map snd) . occurrences . (map edge_id) . concat $ surf)

rotateLeft :: Polygon -> Polygon
rotateLeft [] = []
rotateLeft (x:xs) = xs ++ [x]

rotateRight :: Polygon -> Polygon
rotateRight = reverse . rotateLeft . reverse

simplifyAdjacentEdges' :: Polygon -> Polygon
simplifyAdjacentEdges' (Edge e1 o1:Edge e2 o2:xs)
  | (e1 == e2) && (o1 /= o2) = xs
  | otherwise = (Edge e1 o1 : simplifyAdjacentEdges' (Edge e2 o2 : xs))
simplifyAdjacentEdges' l = l

simplifyAdjacentEdges :: Polygon -> Polygon
simplifyAdjacentEdges =
  simplifyAdjacentEdges' . rotateLeft . simplifyAdjacentEdges'

reflect :: Polygon -> Polygon
reflect = reverse . (map inverse)

connectTwoPolys :: Polygon -> Polygon -> [Polygon]
connectTwoPolys [] p2 = [[], p2]
connectTwoPolys p1 [] = [p1, []]
connectTwoPolys (x:xs) (y:ys)
  | (edge_id x) == (edge_id y) =
    if (orientation x) == (orientation y)
      then [(reflect xs) ++ ys]
      else [xs ++ ys]
  | otherwise = [x : xs, y : ys]
