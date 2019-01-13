module Classification
  ( Edge(Edge)
  , admissible
  , simplifyAdjacentEdges
  , commonEdge
  , connectSurface
  , connectPolys
  , congruent
  ) where

import qualified Data.Map as Map

data Edge = Edge
  { edge_id :: Integer
  , orientation :: Bool
  } deriving (Show, Eq)

inverse :: Edge -> Edge
inverse (Edge a o) = Edge a (not o)

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

congruent' :: Polygon -> Polygon -> Polygon -> Bool
congruent' [] _ _ = False
congruent' (x:xs) q r = ((x : xs) ++ q == r) || congruent' xs (q ++ [x]) r

congruent :: Polygon -> Polygon -> Bool
congruent p q = (congruent' p [] q) || (congruent' (reflect p) [] q)

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

belongsTo :: Edge -> Polygon -> Bool
belongsTo e p = (elem e p) || (elem (inverse e) p)

commonEdge :: Polygon -> Polygon -> Bool
commonEdge [] p2 = False
commonEdge p1 [] = False
commonEdge (x:xs) (y:ys) =
  (x `belongsTo` (y : ys)) || (y `belongsTo` (x : xs)) || commonEdge xs ys

connectPolys' :: Polygon -> Polygon -> Polygon -> Polygon -> Maybe Polygon
connectPolys' [] _ _ _ = Nothing
connectPolys' _ _ [] _ = Nothing
connectPolys' (x:xs) p (y:ys) q
  | (edge_id x) == (edge_id y) =
    if (orientation x) == (orientation y)
      then Just ((reflect p) ++ (reflect xs) ++ ys ++ q)
      else Just (xs ++ p ++ ys ++ q)
  | otherwise =
    case connectPolys' xs (p ++ [x]) (y : ys) q of
      Just l -> Just l
      Nothing ->
        case connectPolys' (x : xs) p ys (q ++ [y]) of
          Just l -> Just l
          Nothing ->
            case connectPolys' xs (p ++ [x]) ys (q ++ [y]) of
              Just l -> Just l
              Nothing -> Nothing

connectPolys :: Polygon -> Polygon -> Maybe Polygon
connectPolys p q = connectPolys' p [] q []

connectSurface :: Surface -> Surface
connectSurface [] = []
connectSurface [p] = [p]
connectSurface (p:q:polys) =
  case connectPolys p q of
    Just r -> connectSurface (r : polys)
    Nothing -> connectSurface (q : connectSurface (p : polys))