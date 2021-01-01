module Classification
  ( Edge (Edge),
    admissible,
    simplifyAdjacentEdges,
    commonEdge,
    connectSurface,
    connectPolys,
    congruent,
    makeTwistedEdgesAdjacent,
  )
where

import Data.List
import qualified Data.Map as Map

data Edge = Edge
  { edge_id :: Integer,
    orientation :: Bool
  }
  deriving (Show, Eq)

type Polygon = [Edge]

type Surface = [Polygon]

-- Change l'orientation d'une arête
inverse :: Edge -> Edge
inverse (Edge a o) = Edge a (not o)

-- Change l'orientation d'un polygone
reflect :: Polygon -> Polygon
reflect = reverse . map inverse

-- Compte le nombre d'occurences de chaque arête
occurrences :: (Ord a) => [a] -> [(a, Int)]
occurrences xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

-- Vérifie si la surface est admissible : chaque arête doit apparaître deux fois
admissible :: Surface -> Bool
admissible surf =
  all ((== 2) . snd) (occurrences $ map edge_id $ concat surf)

-- Permutation circulaire des sommets d'un polygone d'un cran vers la gauche
rotateLeft :: Polygon -> Polygon
rotateLeft [] = []
rotateLeft (x : xs) = xs ++ [x]

-- Permutation circulaire des sommets d'un polygone d'un cran vers la droite
rotateRight :: Polygon -> Polygon
rotateRight = reverse . rotateLeft . reverse

congruent' :: Polygon -> Polygon -> Polygon -> Bool
congruent' [] _ _ = False
congruent' (x : xs) q r = ((x : xs) ++ q == r) || congruent' xs (q ++ [x]) r

-- Vérifie si un polynome peut être obtenu à partir d'un autre par permutation circulaire et/ou changement d'orientation
congruent :: Polygon -> Polygon -> Bool
congruent p q = congruent' p [] q || congruent' (reflect p) [] q

simplifyAdjacentEdges' :: Polygon -> Polygon
simplifyAdjacentEdges' (Edge e1 o1 : Edge e2 o2 : xs)
  | (e1 == e2) && (o1 /= o2) = xs
  | otherwise = Edge e1 o1 : simplifyAdjacentEdges' (Edge e2 o2 : xs)
simplifyAdjacentEdges' l = l

-- Simplifie les arêtes adjacentes et de sens opposés
simplifyAdjacentEdges :: Polygon -> Polygon
simplifyAdjacentEdges =
  simplifyAdjacentEdges' . rotateLeft . simplifyAdjacentEdges'

-- Vérifie si une arête appartient à un polygone
belongsTo :: Edge -> Polygon -> Bool
belongsTo e p = elem e p || elem (inverse e) p

-- Vérifie si deux polygones ont une arête commune
commonEdge :: Polygon -> Polygon -> Bool
commonEdge [] p2 = False
commonEdge p1 [] = False
commonEdge (x : xs) (y : ys) =
  (x `belongsTo` (y : ys)) || (y `belongsTo` (x : xs)) || commonEdge xs ys

connectPolys' :: Polygon -> Polygon -> Polygon -> Polygon -> Maybe Polygon
connectPolys' [] _ _ _ = Nothing
connectPolys' _ _ [] _ = Nothing
connectPolys' (x : xs) p (y : ys) q
  | edge_id x == edge_id y =
    if orientation x == orientation y
      then Just (reflect p ++ reflect xs ++ ys ++ q)
      else Just (xs ++ p ++ ys ++ q)
  | otherwise =
    case connectPolys' xs (p ++ [x]) (y : ys) q of
      Just l -> Just l
      Nothing ->
        case connectPolys' (x : xs) p ys (q ++ [y]) of
          Just l -> Just l
          Nothing -> connectPolys' xs (p ++ [x]) ys (q ++ [y])

-- Réunit deux polygones (renvoie Nothing si pas d'arête commune)
connectPolys :: Polygon -> Polygon -> Maybe Polygon
connectPolys p q = connectPolys' p [] q []

connectSurface' :: Surface -> Surface -> Surface -> Surface
connectSurface' [] [] surf = surf
connectSurface' [] (x : xs) surf = connectSurface' [x] xs surf
connectSurface' (x : xs) [] surf = connectSurface' [] xs (x : surf)
connectSurface' (x : xs) (y : ys) surf =
  case connectPolys x y of
    Nothing -> connectSurface' ((x : xs) ++ [y]) ys surf
    Just p -> connectSurface' [] ((p : xs) ++ ys) surf

-- "Connexifie" une surface
connectSurface :: Surface -> Surface
connectSurface surf = connectSurface' [] surf []

-- http://www.math.uchicago.edu/~may/VIGRE/VIGRE2011/REUPapers/Teo.pdf Step4
makeTwistedEdgesAdjacent' :: Polygon -> Polygon -> Polygon -> Polygon
makeTwistedEdgesAdjacent' [x, y, z, t] l1 l2 = l2
makeTwistedEdgesAdjacent' before [] [] = before
makeTwistedEdgesAdjacent' before [] (x : xs) =
  makeTwistedEdgesAdjacent' before [x] xs
makeTwistedEdgesAdjacent' before (x : xs) [] =
  makeTwistedEdgesAdjacent' (before ++ [x]) [] xs
makeTwistedEdgesAdjacent' before (x : xs) (y : ys) =
  if x == y
    then makeTwistedEdgesAdjacent' ([x, y] ++ before) [] ys ++ reflect xs
    else makeTwistedEdgesAdjacent' before ((x : xs) ++ [y]) ys

makeTwistedEdgesAdjacent :: Polygon -> Polygon
makeTwistedEdgesAdjacent = makeTwistedEdgesAdjacent' [] []

total :: Surface -> Surface
total =
  map (makeTwistedEdgesAdjacent . simplifyAdjacentEdges) . connectSurface
