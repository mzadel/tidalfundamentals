
module DiagramTable where

import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Map (Map,fromList,toList)
import qualified Sound.Tidal.Context as T

outputScaling = 600 :: Double

colourTable :: Map String Int
colourTable = fromList [
     ("a", 0)
    ,("b", 2)
    ,("c", 1)
    ,("d", 7)
    ,("e", 3)
    ,("f", 4)
    ,("bd", 0)]

laneTable :: Map String Int
laneTable = fromList [
     ("a", 0)
    ,("b", 0)
    ,("c", 0)
    ,("d", 1)
    ,("e", 1)]

laneTable2 :: Map String Int
laneTable2 = fromList [
     ("a", 1)
    ,("b", 1)
    ,("c", 1)
    ,("d", 0)
    ,("e", 0)]

patternTable :: Map String (String, Integer)
patternTable = fromList [
    ("basicpattern", ("a b c", 3))
    ,("tildeisarest", ("bd ~ bd ~", 4))
    ,("underscoreelongates", ("a _ c", 3))
    ,("atelongates", ("a@3 b", 4))
    ,("repeateventasterisk", ("a*2 c", 4))
    ,("repeateventbang", ("a!2 c", 3))
    ,("squarebrackets", ("[a b c] [d e]", 4))
    ,("thedot", ("a b c . d e", 4))
    ]

diagramEntry :: (String, (String, Integer)) -> (String, Diagram B)
diagramEntry (label, (patString, numticks)) = (label, diagram)
    where
        diagram = patternDiagram pat numticks colourTable # frame 0.05 # scale outputScaling
        pat = T.s $ T.parseBP_E patString

diagramTableLinear :: [(String, Diagram B)]
diagramTableLinear = [
    ("commaforparallel"
    ,(patternDiagramLinearWithLanes (T.s $ T.parseBP_E "[a b c, d e]") 4 1 laneTable colourTable) # frame 0.05 # scale outputScaling)
    ,("polymetricbraces"
    ,(patternDiagramLinearWithLanes (T.s $ T.parseBP_E "{a b c , d e}") 3 3 laneTable colourTable) # frame 0.05 # scale outputScaling)
    ,("polymetricbracesotherorder"
    ,(patternDiagramLinearWithLanes (T.s $ T.parseBP_E "{d e , a b c}") 2 3 laneTable2 colourTable) # frame 0.05 # scale outputScaling)
    ,("polymetricdividebyeight"
    ,(patternDiagramLinear (T.s $ T.parseBP_E "{a b c d e}%8") 8 2 colourTable) # frame 0.05 # scale outputScaling)
    ,("polymetricdividebyseven"
    ,(patternDiagramLinear (T.s $ T.parseBP_E "{a b c d e}%7") 7 2 colourTable) # frame 0.05 # scale outputScaling)
    ,("anglebrackets"
    ,(patternDiagramLinear (T.s $ T.parseBP_E "<a b c> d <e f>") 3 4 colourTable) # frame 0.05 # scale outputScaling)
    ,("euclideanrhythm"
    ,(patternDiagramLinear (T.s $ T.parseBP_E "a(3,7)") 7 2 colourTable) # frame 0.05 # scale outputScaling)
    ,("slowfunction"
    ,(patternDiagramLinear (T.s $ T.slow 2 $ T.parseBP_E "a a b c") 2 3 colourTable) # frame 0.05 # scale outputScaling)
    ,("slowoneandahalf"
    ,(patternDiagramLinear (T.s $ T.slow 1.5 $ T.parseBP_E "a a b c") 2 3 colourTable) # frame 0.05 # scale outputScaling)
    ,("slowoneandahalfoneeighthticks"
    ,(patternDiagramLinear (T.s $ T.slow 1.5 $ T.parseBP_E "a a b c") 8 3 colourTable) # frame 0.05 # scale outputScaling)
    ,("fastfunction"
    ,(patternDiagramLinear (T.s $ T.fast 3 $ T.parseBP_E "a b c") 9 1 colourTable) # frame 0.05 # scale outputScaling)
    ]

diagramListForMainWith :: [(String, Diagram B)]
diagramListForMainWith = (map diagramEntry $ toList patternTable) ++ diagramTableLinear

