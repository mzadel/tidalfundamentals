
module DiagramTable (diagramListForMainWith) where

import qualified PatternExpressions as PE
import qualified CircularDiagrams as Cir
import qualified LinearDiagrams as Lin
import qualified SignalDiagrams as Sig
import qualified PatternAlgebraDiagrams as PA
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T
import Data.Map (Map,fromList,(!))

outputScaling :: Double
outputScaling = 600

colourFunc :: T.Event T.ValueMap -> Int
colourFunc e = table ! (T.svalue $ T.eventValue e ! "s")
    where
        table :: Map String Int
        table = fromList [
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

colourStringsFunc :: T.Event String -> Int
colourStringsFunc e = table ! T.eventValue e
    where
        table :: Map String Int
        table = fromList [
             ("phi", 1)
            ,("psi", 2)
            ,("tau", 3)]

colourCharsFunc :: T.Event Char -> Int
colourCharsFunc e = table ! T.eventValue e
    where
        table :: Map Char Int
        table = fromList [
             ('a', 0)
            ,('b', 2)
            ,('c', 1)
            ,('d', 7)
            ,('e', 3)
            ,('j', 6)
            ,('k', 4)
            ,('l', 5)
            ,('m', 8)
            ,('n', 9)]

laneCharsFunc :: T.Event Char -> Int
laneCharsFunc e = table ! T.eventValue e
    where
        table :: Map Char Int
        table = fromList [
             ('a', 0)
            ,('b', 0)
            ,('c', 0)
            ,('d', 0)
            ,('j', 1)
            ,('k', 1)
            ,('l', 1)
            ,('m', 2)
            ,('n', 2)]

constColour :: Int -> T.Event a -> Int
constColour colourindex _ = colourindex

laneFunc :: T.Event T.ValueMap -> Int
laneFunc e = table ! (T.svalue $ T.eventValue e ! "s")
    where
        table :: Map String Int
        table = fromList [
             ("a", 0)
            ,("b", 0)
            ,("c", 0)
            ,("d", 1)
            ,("e", 1)]

laneFunc2 :: T.Event T.ValueMap -> Int
laneFunc2 e = table ! (T.svalue $ T.eventValue e ! "s")
    where
        table :: Map String Int
        table = fromList [
             ("a", 1)
            ,("b", 1)
            ,("c", 1)
            ,("d", 0)
            ,("e", 0)]

basicsTable :: [(String, Diagram B)]
basicsTable = [
    ("purecycle"
    ,(Lin.diagramShowValue (PE.purecycleExpr :: T.Pattern String) 1 3 (constColour 0)) # frame 0.05 # scale outputScaling)
    ,("fromListExample"
    ,(Lin.diagramShowValue PE.fromListExampleExpr 1 3 colourStringsFunc) # frame 0.05 # scale outputScaling)
    ,("fastFromListExample"
    ,(Lin.diagramShowValue PE.fastFromListExampleExpr 3 2 colourStringsFunc) # frame 0.05 # scale outputScaling)
    ,("fromMaybesExample"
    ,(Lin.diagramShowValue PE.fromMaybesExampleExpr 3 2 colourStringsFunc) # frame 0.05 # scale outputScaling)
    ,("appendExample"
    ,(Lin.diagramShowValue PE.appendExampleExpr 1 4 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("catExample"
    ,(Lin.diagramShowValue PE.catExampleExpr 1 6 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("fastCatExample"
    ,(Lin.diagramShowValue PE.fastCatExampleExpr 3 2 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("timeCatExample"
    ,(Lin.diagramShowValue PE.timeCatExampleExpr 3 1 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("overlayExample"
    ,(Lin.diagramWithLanesShowValue PE.overlayExampleExpr 4 1 laneCharsFunc colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("stackExample"
    ,(Lin.diagramWithLanesShowValue PE.stackExampleExpr 3 2 laneCharsFunc colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("fastfunction"
    ,(Lin.diagramShowCharValue PE.fastfunctionExpr 3 1 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("slowfunction"
    ,(Lin.diagramShowCharValue PE.slowfunctionExpr 2 3 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("fastgapfunction"
    ,(Lin.diagramShowCharValue PE.fastgapfunctionExpr 3 2 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("compressfunction"
    ,(Lin.diagramShowCharValue PE.compressfunctionExpr 4 3 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("zoomfunction"
    ,(Lin.diagramShowCharValue PE.zoomfunctionExpr 6 2 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("revfunctioninput"
    ,(Lin.diagramShowCharValue PE.revfunctioninputExpr 3 3 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("revfunctionoutput"
    ,(Lin.diagramShowCharValue PE.revfunctionoutputExpr 3 3 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("everyfunction"
    ,(Lin.diagramShowCharValue PE.everyfunctionExpr 1 4 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("whenfunction"
    ,(Lin.diagramShowCharValue PE.whenfunctionExpr 1 4 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("rotRexample"
    ,(Lin.diagramShowCharValue PE.rotRexampleExpr 1 2 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ,("rotRwithfastGap"
    ,(Lin.diagramShowCharValue PE.rotRwithfastGapExpr 8 1 colourCharsFunc) # frame 0.05 # scale outputScaling)
    ]

patternTable :: [(String, (T.Pattern T.ValueMap, Integer))]
patternTable = [
    ("mnocycle", (PE.mnocycleExpr, 3))
    ,("basicpattern", (T.s $ T.parseBP_E PE.basicpatternExpr, 3))
    ,("tildeisarest", (PE.tildeisarestExpr, 4))
    ,("underscoreelongates", (PE.underscoreelongatesExpr, 3))
    ,("atelongates", (PE.atelongatesExpr, 4))
    ,("repeateventasterisk", (PE.repeateventasteriskExpr, 4))
    ,("repeateventbang", (PE.repeateventbangExpr, 3))
    ,("squarebrackets", (PE.squarebracketsExpr, 4))
    ,("thedot", (PE.thedotExpr, 4))
    ]

diagramEntry :: (String, (T.Pattern T.ValueMap, Integer)) -> (String, Diagram B)
diagramEntry (label, (pat, numticks)) = (label, diagram)
    where
        diagram = Cir.diagramLabeledFromSValue pat numticks colourFunc # frame 0.05 # scale outputScaling

diagramTableLinear :: [(String, Diagram B)]
diagramTableLinear = [
    ("commaforparallel"
    ,(Lin.diagramWithLanesLabeledFromSValue PE.commaforparallelExpr 4 1 laneFunc colourFunc) # frame 0.05 # scale outputScaling)
    ,("polymetricbraces"
    ,(Lin.diagramWithLanesLabeledFromSValue PE.polymetricbracesExpr 3 3 laneFunc colourFunc) # frame 0.05 # scale outputScaling)
    ,("polymetricbracesotherorder"
    ,(Lin.diagramWithLanesLabeledFromSValue PE.polymetricbracesotherorderExpr 2 3 laneFunc2 colourFunc) # frame 0.05 # scale outputScaling)
    ,("polymetricdividebyeight"
    ,(Lin.diagramLabeledFromSValue PE.polymetricdividebyeightExpr 8 2 colourFunc) # frame 0.05 # scale outputScaling)
    ,("polymetricdividebyseven"
    ,(Lin.diagramLabeledFromSValue PE.polymetricdividebysevenExpr 7 2 colourFunc) # frame 0.05 # scale outputScaling)
    ,("anglebrackets"
    ,(Lin.diagramLabeledFromSValue PE.anglebracketsExpr 3 4 colourFunc) # frame 0.05 # scale outputScaling)
    ,("euclideanrhythm"
    ,(Lin.diagramLabeledFromSValue PE.euclideanrhythmExpr 7 2 colourFunc) # frame 0.05 # scale outputScaling)
    ,("slowoneandahalf"
    ,(Lin.diagramLabeledFromSValue PE.slowoneandahalfExpr 2 3 colourFunc) # frame 0.05 # scale outputScaling)
    ,("slowoneandahalfoneeighthticks"
    ,(Lin.diagramLabeledFromSValue PE.slowoneandahalfoneeighthticksExpr 8 3 colourFunc) # frame 0.05 # scale outputScaling)
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

diagramTableSignals :: [(String, Diagram B)]
diagramTableSignals = [
    ("sigEvaluatesAtMiddle", Sig.sigEvaluatesAtMiddle # frame 0.05 # scale outputScaling)
    ,("sigToSetPanning", Sig.sigToSetPanning # frame 0.05 # scale outputScaling)
    ]

diagramListForMainWith :: [(String, Diagram B)]
diagramListForMainWith = basicsTable ++ (map diagramEntry patternTable) ++ diagramTableLinear ++ diagramTablePatternAlgebra ++ diagramTableSignals

