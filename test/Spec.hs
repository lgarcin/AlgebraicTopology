import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Data.List
import Data.Maybe
import Data.Ord

import Classification

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)" []

qcProps = testGroup "(checked by QuickCheck)" []

unitTests =
  testGroup
    "Unit tests"
    [ testCase "simplifyAdjacentEdges" $
      assertEqual [] (simplifyAdjacentEdges [Edge 1 True, Edge 1 False]) []
    , testCase "simplifyAdjacentEdges" $
      assertEqual
        []
        (simplifyAdjacentEdges [Edge 1 True, Edge 1 False, Edge 2 False])
        [Edge 2 False]
    , testCase "simplifyAdjacentEdges" $
      assertEqual
        []
        (simplifyAdjacentEdges [Edge 1 True, Edge 2 True, Edge 1 False])
        [Edge 2 True]
    , testCase "congruent" $
      assertEqual
        []
        (congruent
           [Edge 1 True, Edge 2 False, Edge 3 True]
           [Edge 2 False, Edge 3 True, Edge 1 True])
        True
    , testCase "congruent" $
      assertEqual
        []
        (congruent
           [Edge 3 False, Edge 2 True, Edge 1 False]
           [Edge 2 False, Edge 3 True, Edge 1 True])
        True
    , testCase "congruent" $
      assertEqual
        []
        (congruent
           [Edge 1 False, Edge 2 True, Edge 3 False]
           [Edge 2 False, Edge 3 True, Edge 1 True])
        False
    , testCase "connectPolys" $
      assertEqual
        []
        (any
           (\x -> x)
           (map
              (congruent
                 (fromJust $
                  connectPolys
                    [Edge 1 False, Edge 2 True, Edge 3 False]
                    [Edge 2 False, Edge 3 True, Edge 1 True]))
              [ [Edge 2 True, Edge 3 False, Edge 2 False, Edge 3 True]
              , [Edge 3 False, Edge 1 False, Edge 3 True, Edge 1 True]
              , [Edge 1 False, Edge 2 True, Edge 1 True, Edge 2 True]
              ]))
        True
    ]
