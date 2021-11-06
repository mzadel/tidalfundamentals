
module DiagramTable (diagramListForMainWith) where

import qualified CircularDiagrams as Cir
import qualified LinearDiagrams as Lin
import qualified PatternAlgebraDiagrams as PA
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T
import Data.Map (Map,fromList,toList)

outputScaling = 600 :: Double

colourTable :: Map String Int
colourTable = fromList [
     ("a", 0)
    ,("b", 2)
    ,("c", 1)
    ,("d", 7)
    ,("e", 3)
    ,("f", 4)
    ,("m", 0)
    ,("n", 2)
    ,("o", 1)
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
    ("mnocycle", ("m n o", 3))
    ,("basicpattern", ("a b c", 3))
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
        diagram = Cir.diagram pat numticks colourTable # frame 0.05 # scale outputScaling
        pat = T.s $ T.parseBP_E patString

diagramTableLinear :: [(String, Diagram B)]
diagramTableLinear = [
    ("commaforparallel"
    ,(Lin.diagramWithLanesLabeledFromSValue (T.s $ T.parseBP_E "[a b c, d e]") 4 1 laneTable colourTable) # frame 0.05 # scale outputScaling)
    ,("polymetricbraces"
    ,(Lin.diagramWithLanesLabeledFromSValue (T.s $ T.parseBP_E "{a b c , d e}") 3 3 laneTable colourTable) # frame 0.05 # scale outputScaling)
    ,("polymetricbracesotherorder"
    ,(Lin.diagramWithLanesLabeledFromSValue (T.s $ T.parseBP_E "{d e , a b c}") 2 3 laneTable2 colourTable) # frame 0.05 # scale outputScaling)
    ,("polymetricdividebyeight"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.parseBP_E "{a b c d e}%8") 8 2 colourTable) # frame 0.05 # scale outputScaling)
    ,("polymetricdividebyseven"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.parseBP_E "{a b c d e}%7") 7 2 colourTable) # frame 0.05 # scale outputScaling)
    ,("anglebrackets"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.parseBP_E "<a b c> d <e f>") 3 4 colourTable) # frame 0.05 # scale outputScaling)
    ,("euclideanrhythm"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.parseBP_E "a(3,7)") 7 2 colourTable) # frame 0.05 # scale outputScaling)
    ,("slowfunction"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.slow 2 $ T.parseBP_E "a a b c") 2 3 colourTable) # frame 0.05 # scale outputScaling)
    ,("slowoneandahalf"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.slow 1.5 $ T.parseBP_E "a a b c") 2 3 colourTable) # frame 0.05 # scale outputScaling)
    ,("slowoneandahalfoneeighthticks"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.slow 1.5 $ T.parseBP_E "a a b c") 8 3 colourTable) # frame 0.05 # scale outputScaling)
    ,("fastfunction"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.fast 3 $ T.parseBP_E "a b c") 9 1 colourTable) # frame 0.05 # scale outputScaling)
    ,("fastgapfunction"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.fastGap 3 $ T.parseBP_E "a b c") 3 2 colourTable) # frame 0.05 # scale outputScaling)
    ,("compressfunction"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.compress (1/4,1/2) $ T.parseBP_E "a b c") 4 3 colourTable) # frame 0.05 # scale outputScaling)
    ,("zoomfunction"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.zoom (1/4,3/4) $ T.parseBP_E "a b c") 6 2 colourTable) # frame 0.05 # scale outputScaling)
    ,("revfunctioninput"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.slow 2 $ T.parseBP_E "a ~ b ~ ~ c") 3 3 colourTable) # frame 0.05 # scale outputScaling)
    ,("revfunctionoutput"
    ,(Lin.diagramLabeledFromSValue (T.s $ T.rev $ T.slow 2 $ T.parseBP_E "a ~ b ~ ~ c") 3 3 colourTable) # frame 0.05 # scale outputScaling)
    ]

diagramTablePatternAlgebra :: [(String, Diagram B)]
diagramTablePatternAlgebra = [
    ("leftPlusExample1", PA.leftPlusExample1 # frame 0.05 # scale outputScaling)
    ,("leftPlusExample2", PA.leftPlusExample2 # frame 0.05 # scale outputScaling)
    ,("leftPlusExample3", PA.leftPlusExample3 # frame 0.05 # scale outputScaling)
    ,("rightPlusExample1", PA.rightPlusExample1 # frame 0.05 # scale outputScaling)
    ,("rightPlusExample2", PA.rightPlusExample2 # frame 0.05 # scale outputScaling)
    ,("rightPlusExample3", PA.rightPlusExample3 # frame 0.05 # scale outputScaling)
    ,("bothPlusExample1", PA.bothPlusExample1 # frame 0.05 # scale outputScaling)
    ,("bothPlusExample2", PA.bothPlusExample2 # frame 0.05 # scale outputScaling)
    ,("bothPlusExample3", PA.bothPlusExample3 # frame 0.05 # scale outputScaling)
    ,("justPlusExample1", PA.justPlusExample1 # frame 0.05 # scale outputScaling)
    ,("valueAlgebraMapDiagram", PA.valueAlgebraMapDiagram # frame 0.05 # scale outputScaling)
    ]

diagramListForMainWith :: [(String, Diagram B)]
diagramListForMainWith = (map diagramEntry $ toList patternTable) ++ diagramTableLinear ++ diagramTablePatternAlgebra

