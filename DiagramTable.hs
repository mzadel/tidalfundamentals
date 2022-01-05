
module DiagramTable (diagramListForMainWith) where

import Shared (showDoubleTruncated)
import qualified PatternExpressions as PE
import qualified CircularDiagrams as Cir
import qualified LinearDiagrams.LinearDiagrams as Lin
import qualified LinearDiagrams.Curve as Curve
import qualified SignalDiagrams as Sig
import qualified PatternAlgebraDiagrams as PA
import qualified QueryDiagrams as Q
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T
import Data.Map ((!))

frameAndScale :: Diagram B -> Diagram B
frameAndScale diagram = diagram # frame 0.05 # scale 600

colourStringsFunc :: T.Event String -> Int
colourStringsFunc e = colour
    where
        colour = case T.eventValue e of
            "phi" -> 1
            "psi" -> 2
            "tau" -> 3
            _ -> 0

colourCharsFunc :: T.Event Char -> Int
colourCharsFunc e = colour
    where
        colour = case T.eventValue e of
            'a' -> 0
            'b' -> 2
            'c' -> 1
            'd' -> 7
            'e' -> 3
            'f' -> 4
            'j' -> 6
            'k' -> 4
            'l' -> 5
            'm' -> 8
            'n' -> 9
            _ -> 0

laneCharsFunc :: T.Event Char -> Int
laneCharsFunc e = lane
    where
        lane = case T.eventValue e of
            'a' -> 0
            'b' -> 0
            'c' -> 0
            'd' -> 0
            'j' -> 1
            'k' -> 1
            'l' -> 1
            'm' -> 2
            'n' -> 2
            _ -> 0

constColour :: Int -> T.Event a -> Int
constColour colourindex _ = colourindex

laneFunc :: T.Event Char -> Int
laneFunc e = lane
    where
        lane = case T.eventValue e of
            'a' -> 0
            'b' -> 0
            'c' -> 0
            'd' -> 1
            'e' -> 1
            _ -> 0

laneFunc2 :: T.Event Char -> Int
laneFunc2 e = lane
    where
        lane = case T.eventValue e of
            'a' -> 1
            'b' -> 1
            'c' -> 1
            'd' -> 0
            'e' -> 0
            _ -> 0

basicsTable :: [(String, Diagram B)]
basicsTable = [
    ("purecycle"
    ,(Lin.diagramShowValue (PE.purecycleExpr :: T.Pattern String) 1 3 (constColour 0)) # frameAndScale)
    ,("fromListExample"
    ,(Lin.diagramShowValue PE.fromListExampleExpr 1 3 colourStringsFunc) # frameAndScale)
    ,("fastFromListExample"
    ,(Lin.diagramShowValue PE.fastFromListExampleExpr 3 2 colourStringsFunc) # frameAndScale)
    ,("fastFromListExampleCircular"
    ,(Cir.diagramShowValue PE.fastFromListExampleCircularExpr 3 colourStringsFunc) # frameAndScale)
    ,("fromMaybesExample"
    ,(Cir.diagramShowValue PE.fromMaybesExampleExpr 3 colourStringsFunc) # frameAndScale)
    ,("appendExample"
    ,(Lin.diagramShowValue PE.appendExampleExpr 1 4 colourCharsFunc) # frameAndScale)
    ,("catExample"
    ,(Lin.diagramShowValue PE.catExampleExpr 1 6 colourCharsFunc) # frameAndScale)
    ,("fastCatExample"
    ,(Lin.diagramShowValue PE.fastCatExampleExpr 3 2 colourCharsFunc) # frameAndScale)
    ,("timeCatExample"
    ,(Lin.diagramShowValue PE.timeCatExampleExpr 3 1 colourCharsFunc) # frameAndScale)
    ,("overlayExample"
    ,(Lin.diagramWithLanesShowValue PE.overlayExampleExpr 4 1 laneCharsFunc colourCharsFunc) # frameAndScale)
    ,("stackExample"
    ,(Lin.diagramWithLanesShowValue PE.stackExampleExpr 3 2 laneCharsFunc colourCharsFunc) # frameAndScale)
    ,("fastfunction"
    ,(Lin.diagramShowCharValue PE.fastfunctionExpr 3 1 colourCharsFunc) # frameAndScale)
    ,("slowfunction"
    ,(Lin.diagramShowCharValue PE.slowfunctionExpr 2 3 colourCharsFunc) # frameAndScale)
    ,("fastgapfunction"
    ,(Lin.diagramShowCharValue PE.fastgapfunctionExpr 3 2 colourCharsFunc) # frameAndScale)
    ,("compressfunction"
    ,(Lin.diagramShowCharValue PE.compressfunctionExpr 4 3 colourCharsFunc) # frameAndScale)
    ,("zoomfunction"
    ,(Lin.diagramShowCharValue PE.zoomfunctionExpr 6 2 colourCharsFunc) # frameAndScale)
    ,("revfunctioninput"
    ,(Lin.diagramShowCharValue PE.revfunctioninputExpr 3 3 colourCharsFunc) # frameAndScale)
    ,("revfunctionoutput"
    ,(Lin.diagramShowCharValue PE.revfunctionoutputExpr 3 3 colourCharsFunc) # frameAndScale)
    ,("everyfunction"
    ,(Lin.diagramShowCharValue PE.everyfunctionExpr 1 4 colourCharsFunc) # frameAndScale)
    ,("whenfunction"
    ,(Lin.diagramShowCharValue PE.whenfunctionExpr 1 4 colourCharsFunc) # frameAndScale)
    ,("rotRexample"
    ,(Lin.diagramShowCharValue PE.rotRexampleExpr 1 2 colourCharsFunc) # frameAndScale)
    ,("rotRwithfastGap"
    ,(Lin.diagramShowCharValue PE.rotRwithfastGapExpr 8 1 colourCharsFunc) # frameAndScale)
    ]

patternTable :: [(String, (T.Pattern Char, Integer))]
patternTable = [
    ("basicpattern", (T.parseBP_E PE.basicpatternExpr :: T.Pattern Char, 3))
    ,("tildeisarest", (T.parseBP_E PE.tildeisarestExpr :: T.Pattern Char, 4))
    ,("underscoreelongates", (T.parseBP_E PE.underscoreelongatesExpr :: T.Pattern Char, 3))
    ,("atelongates", (T.parseBP_E PE.atelongatesExpr :: T.Pattern Char, 4))
    ,("repeateventasterisk", (T.parseBP_E PE.repeateventasteriskExpr :: T.Pattern Char, 4))
    ,("repeateventbang", (T.parseBP_E PE.repeateventbangExpr :: T.Pattern Char, 3))
    ,("squarebrackets", (T.parseBP_E PE.squarebracketsExpr :: T.Pattern Char, 4))
    ,("thedot", (T.parseBP_E PE.thedotExpr :: T.Pattern Char, 4))
    ]

diagramEntry :: (String, (T.Pattern Char, Integer)) -> (String, Diagram B)
diagramEntry (label, (pat, numticks)) = (label, diagram)
    where
        diagram = Cir.diagramShowCharValue pat numticks colourCharsFunc # frameAndScale

diagramTableLinear :: [(String, Diagram B)]
diagramTableLinear = [
    ("commaforparallel"
    ,(Lin.diagramWithLanesShowChar (T.parseBP_E PE.commaforparallelExpr :: T.Pattern Char) 4 1 laneFunc colourCharsFunc) # frameAndScale)
    ,("polymetricbraces"
    ,(Lin.diagramWithLanesShowChar (T.parseBP_E PE.polymetricbracesExpr :: T.Pattern Char) 3 3 laneFunc colourCharsFunc) # frameAndScale)
    ,("polymetricbracesotherorder"
    ,(Lin.diagramWithLanesShowChar (T.parseBP_E PE.polymetricbracesotherorderExpr :: T.Pattern Char) 2 3 laneFunc2 colourCharsFunc) # frameAndScale)
    ,("polymetricdividebyeight"
    ,(Lin.diagramShowCharValue (T.parseBP_E PE.polymetricdividebyeightExpr :: T.Pattern Char) 8 2 colourCharsFunc) # frameAndScale)
    ,("polymetricdividebyseven"
    ,(Lin.diagramShowCharValue (T.parseBP_E PE.polymetricdividebysevenExpr :: T.Pattern Char) 7 2 colourCharsFunc) # frameAndScale)
    ,("anglebrackets"
    ,(Lin.diagramShowCharValue (T.parseBP_E PE.anglebracketsExpr :: T.Pattern Char) 3 4 colourCharsFunc) # frameAndScale)
    ,("euclideanrhythm"
    ,(Lin.diagramShowCharValue (T.parseBP_E PE.euclideanrhythmExpr :: T.Pattern Char) 7 2 colourCharsFunc) # frameAndScale)
    ,("slowoneandahalf"
    ,(Lin.diagramShowCharValue PE.slowoneandahalfExpr 2 3 colourCharsFunc) # frameAndScale)
    ]

diagramTablePatternAlgebra :: [(String, Diagram B)]
diagramTablePatternAlgebra = [
    ("numberpatternmin", Lin.diagramFromWholes show (PE.numberpatternminExpr :: T.Pattern Int) 1 # frameAndScale)
    ,("numberpatternmax", Lin.diagramFromWholes show (PE.numberpatternmaxExpr :: T.Pattern Int) 1 # frameAndScale)
    ,("numberpatternplus", Lin.diagramFromWholes show (PE.numberpatternplusExpr :: T.Pattern Int) 1 # frameAndScale)
    ,("numberpatterntimes", Lin.diagramFromWholes show (PE.numberpatterntimesExpr :: T.Pattern Int) 1 # frameAndScale)
    ,("numberpatternmod", Lin.diagramFromWholes show (PE.numberpatternmodExpr :: T.Pattern Int) 1 # frameAndScale)
    ,("numberpatternsqrt", Lin.diagramFromWholes showDoubleTruncated PE.numberpatternsqrtExpr 1 # frameAndScale)
    ,("additionexample", PA.additionexample # frameAndScale)
    ,("leftPlusExample1", PA.leftPlusExample1 # frameAndScale)
    ,("leftPlusExample2", PA.leftPlusExample2 # frameAndScale)
    ,("leftPlusExample3", PA.leftPlusExample3 # frameAndScale)
    ,("rightPlusExample1", PA.rightPlusExample1 # frameAndScale)
    ,("rightPlusExample2", PA.rightPlusExample2 # frameAndScale)
    ,("rightPlusExample3", PA.rightPlusExample3 # frameAndScale)
    ,("bothPlusExample1", PA.bothPlusExample1 # frameAndScale)
    ,("bothPlusExample2", PA.bothPlusExample2 # frameAndScale)
    ,("bothPlusExample3", PA.bothPlusExample3 # frameAndScale)
    ,("justPlusExample1", PA.justPlusExample1 # frameAndScale)
    ,("applicativeboth", Lin.diagramFromWholes showDoubleTruncated (PE.applicativebothExpr :: T.Pattern Double) 1 # frameAndScale)
    ,("applicativeleft", Lin.diagramFromWholes showDoubleTruncated (PE.applicativeleftExpr :: T.Pattern Double) 1 # frameAndScale)
    ,("applicativeright", Lin.diagramFromWholes showDoubleTruncated (PE.applicativerightExpr :: T.Pattern Double) 1 # frameAndScale)
    ,("filtervaluesexample", Lin.diagramFromWholes show PE.filtervaluesexampleExpr 1 # frameAndScale)
    ,("valueAlgebraMapDiagram", PA.valueAlgebraMapDiagram # frameAndScale)
    ]

diagramTableSignals :: [(String, Diagram B)]
diagramTableSignals = [
    ("sigEvaluatesAtMiddle", Sig.sigEvaluatesAtMiddle # frameAndScale)
    ,("sigToSetPanning", Sig.sigToSetPanning # frameAndScale)
    ]

diagramTableQueries :: [(String, Diagram B)]
diagramTableQueries = [
    ("queryFullCycle", Q.example PE.queryFullCyclePatternExpr PE.queryFullCycleArcExpr colourCharsFunc # frameAndScale)
    ,("queryHalfCycle", Q.example PE.queryHalfCyclePatternExpr PE.queryHalfCycleArcExpr colourCharsFunc # frameAndScale)
    ,("queryQuarterCycle", Q.example PE.queryQuarterCyclePatternExpr PE.queryQuarterCycleArcExpr colourCharsFunc # frameAndScale)
    ,("queryEmptySpace", Q.example PE.queryEmptySpacePatternExpr PE.queryEmptySpaceArcExpr colourCharsFunc # frameAndScale)
    ,("queryOverlapOneItemExactly", Q.example PE.queryOverlapOneItemExactlyPatternExpr PE.queryOverlapOneItemExactlyArcExpr colourCharsFunc # frameAndScale)
    ,("queryOverlapOneItemExactly2", Q.example PE.queryOverlapOneItemExactly2PatternExpr PE.queryOverlapOneItemExactly2ArcExpr colourCharsFunc # frameAndScale)
    ,("queryOverlapTwoItemsExactly", Q.example PE.queryOverlapTwoItemsExactlyPatternExpr PE.queryOverlapTwoItemsExactlyArcExpr colourCharsFunc # frameAndScale)
    ,("queryOverlapTwoItems", Q.example PE.queryOverlapTwoItemsPatternExpr PE.queryOverlapTwoItemsArcExpr colourCharsFunc # frameAndScale)
    ,("queryTwoItemsZeroWidth", Q.example PE.queryTwoItemsZeroWidthPatternExpr PE.queryTwoItemsZeroWidthArcExpr colourCharsFunc # frameAndScale)
    ,("queryOneItemZeroWidth", Q.example PE.queryOneItemZeroWidthPatternExpr PE.queryOneItemZeroWidthArcExpr colourCharsFunc # frameAndScale)
    ,("simplesine", Curve.curveDiagram PE.simplesineExpr 1 # frameAndScale)
    ,("querycontinuouspattern", Q.continuousPatternExample PE.querycontinuouspatternPatternExpr PE.querycontinuouspatternArcExpr # frameAndScale)
    ,("querycontinuouspatternshowpoint", Q.continuousPatternExampleShowPoint PE.querycontinuouspatternshowpointPatternExpr PE.querycontinuouspatternshowpointArcExpr # frameAndScale)
    ,("querycontinuouspatternsamplesatmidpoint", Q.continuousPatternExampleShowPoint PE.querycontinuouspatternsamplesatmidpointPatternExpr PE.querycontinuouspatternsamplesatmidpointArcExpr # frameAndScale)
    ,("querycontinuouspatternzwarc", Q.continuousPatternExampleShowPoint PE.querycontinuouspatternzwarcPatternExpr PE.querycontinuouspatternzwarcArcExpr # frameAndScale)
    ]

diagramTableJoins :: [(String, Diagram B)]
diagramTableJoins = [
    ("unwrapexample", Lin.diagramShowCharValue PE.unwrapexampleExpr 6 1 colourCharsFunc # frameAndScale)
    ,("innerjoinexample", Lin.diagramShowCharValue PE.innerjoinexampleExpr 6 1 colourCharsFunc # frameAndScale)
    ,("outerjoinexample", Lin.diagramShowCharValue PE.outerjoinexampleExpr 2 1 colourCharsFunc # frameAndScale)
    ,("squeezejoinexample", Lin.diagramShowCharValue PE.squeezejoinexampleExpr 2 1 colourCharsFunc # frameAndScale)
    ]

lanesFromPanning :: T.Event T.ValueMap -> Int
lanesFromPanning e = case T.fvalue (T.value e ! "pan") of
    0.0 -> 0
    1.0 -> 1
    _ -> 2

diagramTableUI :: [(String, Diagram B)]
diagramTableUI = [
    ("juxexample", Lin.diagramWithLanesFromWholes show PE.juxexampleExpr 1 lanesFromPanning # frameAndScale)
    ,("shuffleexample", Lin.diagramFromWholes show PE.shuffleexampleExpr 3 # frameAndScale)
    ]

diagramListForMainWith :: [(String, Diagram B)]
diagramListForMainWith = basicsTable ++ (map diagramEntry patternTable) ++ diagramTableLinear ++ diagramTablePatternAlgebra ++ diagramTableSignals ++ diagramTableQueries ++ diagramTableJoins ++ diagramTableUI

