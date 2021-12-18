
module LinearDiagrams.LinearDiagrams (diagramShowValue,diagramWithLanesShowValue,diagramLabeledFromSValue,diagramShowCharValue,diagramWithLanesLabeledFromSValue,diagramFromWholes,curveDiagram,curveDiagramLabeledPoint) where

import Shared
import LinearDiagrams.Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.ColorSet (Brightness(Light,Dark))
import qualified Sound.Tidal.Context as T
import Data.Ratio
import qualified Data.Map as M ((!))
import Control.Applicative (ZipList(ZipList,getZipList))

tickMark :: Rational -> Diagram B
tickMark tickLoc = mark
    where
        tickMarkSize = 0.016
        point = p2 (fromRational tickLoc, 0)
        mark = vrule tickMarkSize # moveTo point

tickMarkLabel :: Rational -> Diagram B
tickMarkLabel tickLoc = label
    where
        labelPoint = p2 (fromRational tickLoc, 0)
        labelText = ratioToString tickLoc
        label = text labelText # fontSize tickMarkLabelSize # alignB # moveTo labelPoint

curveGeometry :: T.Pattern Double -> Diagram B
curveGeometry ctspattern = fromVertices (getZipList curvepoints)
    where
        curvepoints :: ZipList (P2 Double)
        curvepoints = (^&) <$> xs <*> ys
        xs :: ZipList Double
        xs = fromRational <$> ts
        ys :: ZipList Double
        ys = (curveValueAtTime ctspattern) <$> ts
        ts :: ZipList T.Time
        ts = ZipList [0, deltat .. 1]
        deltat = 1 % 100

diagramShowValue :: (Show a) => T.Pattern a -> Integer -> Rational -> (T.Event a -> Int) -> Diagram B
diagramShowValue tidalPattern ticksPerCycle queryEnd colourFunc = diagramWithLanesShowValue tidalPattern ticksPerCycle queryEnd laneFunc colourFunc
    where
        laneFunc _ = 0

diagramWithLanesShowValue :: (Show a) => T.Pattern a -> Integer -> Rational -> (T.Event a -> Int) -> (T.Event a -> Int) -> Diagram B
diagramWithLanesShowValue = diagramWithLanes (show . T.eventValue)

diagramWithLanes :: (T.Event a -> String) -> T.Pattern a -> Integer -> Rational -> (T.Event a -> Int) -> (T.Event a -> Int) -> Diagram B
diagramWithLanes formatLabel tidalPattern ticksPerCycle queryEnd laneFunc colourFunc =
        vsep linearDiagramVerticalPadding [
            mconcat (map tickMarkLabel tickLocList)
            ,mconcat (map tickMark tickLocList)
            ,(mconcat patterneventlabels <> mconcat patternevents)
            ]
    where
        events = ZipList $ T.queryArc tidalPattern (T.Arc 0 queryEnd)
        --
        patternevents = getZipList $ laneTranslations <*> (boxStyles <*> boxgeometries)
        patterneventlabels = getZipList $ laneTranslations <*> (labelStyles <*> labelgeometries)
        --
        tickLocList = tickMarkLocations (1%ticksPerCycle) queryEnd
        --
        labels = formatLabel <$> events
        colours = colourFunc <$> events
        lanes = laneFunc <$> events
        starts = T.eventPartStart <$> events
        stops = T.eventPartStop <$> events
        --
        boxgeometries :: ZipList (Diagram B)
        boxgeometries = boxGeometry <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> starts
        boxStyles :: ZipList (Diagram B -> Diagram B)
        boxStyles = style Dark <$> colours
        labelStyles :: ZipList (Diagram B -> Diagram B)
        labelStyles = style Light <$> colours
        laneTranslations :: ZipList (Diagram B -> Diagram B)
        laneTranslations = moveToLane <$> lanes

diagramLabeledFromSValue :: T.ControlPattern -> Integer -> Rational -> (T.Event T.ValueMap -> Int) -> Diagram B
diagramLabeledFromSValue tidalPattern ticksPerCycle queryEnd colourFunc = diagramWithLanesLabeledFromSValue tidalPattern ticksPerCycle queryEnd laneFunc colourFunc
    where
        laneFunc _ = 0

diagramShowCharValue :: T.Pattern Char -> Integer -> Rational -> (T.Event Char -> Int) -> Diagram B
diagramShowCharValue tidalPattern ticksPerCycle queryEnd colourFunc = diagramWithLanes showFunction tidalPattern ticksPerCycle queryEnd laneFunc colourFunc
    where
        showFunction = charToString . T.eventValue
        laneFunc _ = 0

-- lanes are numbered from zero, starting at the top
moveToLane :: Int -> Diagram B -> Diagram B
moveToLane lane = translateY ((fromIntegral $ -lane) * eventWidth)

diagramWithLanesLabeledFromSValue :: T.ControlPattern -> Integer -> Rational -> (T.Event T.ValueMap -> Int) -> (T.Event T.ValueMap -> Int) -> Diagram B
diagramWithLanesLabeledFromSValue = diagramWithLanes getLabel
    where
        getLabel :: T.Event T.ValueMap -> String
        getLabel e = T.svalue $ T.eventValue e M.! "s"

diagramFromWholes :: (a -> String) -> T.Pattern a -> Rational -> Diagram B
diagramFromWholes formatLabel tidalPattern queryEnd =
    mconcat patterneventlabels <> mconcat patternevents
    where
        events = T.queryArc tidalPattern (T.Arc 0 queryEnd)
        eventswithonsets = ZipList $ filter T.eventHasOnset events
        --
        patternevents = getZipList $ boxgeometries
        patterneventlabels = getZipList $ labelgeometries
        --
        getLabel = formatLabel . T.eventValue
        --
        labels = getLabel <$> eventswithonsets
        starts = T.wholeStart <$> eventswithonsets
        stops = T.wholeStop <$> eventswithonsets
        --
        boxgeometries :: ZipList (Diagram B)
        boxgeometries = boxGeometry <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> starts

curveDiagramHeight :: Double
curveDiagramHeight = 0.2

curveDiagramTickMarkOffset :: Double
curveDiagramTickMarkOffset = fromRational (-eventLabelInset)

curveDiagram :: T.Pattern Double -> Integer -> Diagram B
curveDiagram ctsPattern ticksPerCycle =
    curveandaxes # scaleY curveDiagramHeight
    <> mconcat (map tickMark tickLocList)
    <> mconcat (map tickMarkLabel tickLocList) # translateY curveDiagramTickMarkOffset
    where
        curveandaxes =
            fromOffsets [unitY]
            <> fromOffsets [unitX]
            <> curveGeometry ctsPattern
        tickLocList = tickMarkLocations (1%ticksPerCycle) 1

curveDiagramLabeledPoint :: P2 Double -> String -> Diagram B
curveDiagramLabeledPoint pos labelText = (thedot <> label) # translate (scaledpos .-. origin)
    where
        scaledpos = pos # scaleY curveDiagramHeight
        thedot = circle 0.0075 # fc red # lw none
        label = alignedText 0 0.5 labelText # fontSize eventLabelSize # translateX (fromRational eventLabelInset)

