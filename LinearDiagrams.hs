
module LinearDiagrams (diagramShowValue,diagramWithLanesShowValue,diagramLabeledFromSValue,diagramShowCharValue,diagramWithLanesLabeledFromSValue,diagramFromWholes,curveDiagram,curveDiagramLabeledPoint,arcDiagram,queryDiagram) where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.ColorSet (Brightness(Light,Dark),d3Colors2)
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

boxGeometry :: Rational -> Rational -> Diagram B
boxGeometry startLoc stopLoc = rect (fromRational $ stopLoc-startLoc) eventWidth # alignL # moveTo ((fromRational $ startLoc) ^& 0)

labelGeometry :: String -> Rational -> Diagram B
labelGeometry labelString boxStartLoc = label
    where
        label = alignedText 0 0.5 labelString # fontSize eventLabelSize # moveTo labelPoint
        labelPoint = (fromRational (boxStartLoc + eventLabelInset)) ^& 0

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

arcGeometry :: Rational -> Rational -> Diagram B
arcGeometry startLoc stopLoc = (strokeP leftedge <> strokeP rightedge <> thearrow) # alignL # moveTo ((fromRational startLoc) ^& 0)
    where
        ysize = eventWidth
        xsize = fromRational $ stopLoc - startLoc
        leftedge = vrule ysize
        rightedge = vrule ysize # translateX xsize
        thearrow = arrowV' arrowopts (xsize ^& 0) # translateY (-ysize * 0.35)
        arrowopts = with
            & arrowHead .~ dart & headLength .~ small
            & arrowTail .~ dart' & tailLength .~ small

arcLabelGeometry :: String -> Rational -> Rational -> Diagram B
arcLabelGeometry labelString arcStartLoc arcStopLoc = label
    where
        label = alignedText 0.5 0.5 labelString # fontSize eventLabelSize # moveTo labelCentre
        labelCentre = (fromRational (arcStartLoc + arcStopLoc) / 2) ^& 0

designatorSize :: Measure Double
designatorSize = eventLabelSize * 0.75

designatorInset :: Rational
designatorInset = eventLabelInset / 4

designatorVerticalOffset :: Double
designatorVerticalOffset = -eventWidth / 2

partDesignatorGeometry :: Rational -> Diagram B
partDesignatorGeometry boxStartLoc = label
    where
        label = topLeftText "part" # fontSize designatorSize # moveTo labelPoint
        labelPoint = (fromRational $ boxStartLoc + designatorInset) ^& designatorVerticalOffset

wholeDesignatorGeometryAtLeft :: Rational -> Diagram B
wholeDesignatorGeometryAtLeft boxStartLoc = label
    where
        label = topLeftText "whole" # fontSize designatorSize # moveTo labelPoint
        labelPoint = (fromRational $ boxStartLoc + designatorInset) ^& designatorVerticalOffset

wholeDesignatorGeometryAtRight :: Rational -> Diagram B
wholeDesignatorGeometryAtRight boxEndLoc = label
    where
        label = alignedText 1 1 "whole" # fontSize designatorSize # moveTo labelPoint
        labelPoint = (fromRational $ boxEndLoc - designatorInset) ^& designatorVerticalOffset

wholeDesignatorGeometry :: T.Event Char -> Diagram B
wholeDesignatorGeometry (T.Event _ Nothing _ _) = mempty
wholeDesignatorGeometry (T.Event _ (Just thewhole) thepart _) = designatorgeometry
    where
        designatorgeometry
            | (T.start thewhole < T.start thepart) = wholeDesignatorGeometryAtLeft (T.start thewhole)
            | (T.stop thepart < T.stop thewhole) = wholeDesignatorGeometryAtRight (T.stop thewhole)
            -- otherwise thewhole == thepart
            | otherwise = wholeDesignatorGeometryAtRight (T.stop thewhole)

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

charToString :: Char -> String
charToString c = [c]

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

arcDiagram :: [T.Arc] -> Diagram B
arcDiagram arcs =
    mconcat arclabels <> mconcat arcdrawings
    where
        arcdrawings = getZipList $ arcgeometries
        arclabels = getZipList $ labelgeometries
        --
        getLabel :: T.Arc -> String
        getLabel a = "Arc " ++ (show $ startdouble) ++ " " ++ (show $ stopdouble)
            where
                startdouble = fromRational $ T.start a :: Double
                stopdouble = fromRational $ T.stop a :: Double
        --
        labels = getLabel <$> ZipList arcs
        starts = T.start <$> ZipList arcs
        stops = T.stop <$> ZipList arcs
        --
        arcgeometries :: ZipList (Diagram B)
        arcgeometries = arcGeometry <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = arcLabelGeometry <$> labels <*> starts <*> stops

queryDiagram :: [T.Event Char] -> (T.Event Char -> Int) -> Diagram B
queryDiagram events colourFunc =
    mconcat valuelabels
    <> mconcat wholedesignators
    <> mconcat partdesignators
    <> mconcat partdrawings
    <> mconcat wholedrawings
    where
        wholedrawings = getZipList $ wholestyles <*> wholeboxgeometries
        partdrawings = getZipList $ partstyles <*> partboxgeometries
        valuelabels = getZipList $ labelstyles <*> labelgeometries
        wholedesignators = getZipList $ wholedesignatorstyles <*> wholedesignatorgeometries
        partdesignators = getZipList $ partdesignatorstyles <*> partdesignatorgeometries
        --
        getLabel = charToString . T.eventValue
        --
        labels = getLabel <$> ZipList events
        wholestarts = T.wholeStart <$> ZipList events
        wholestops = T.wholeStop <$> ZipList events
        partstarts = T.eventPartStart <$> ZipList events
        partstops = T.eventPartStop <$> ZipList events
        colours = colourFunc <$> ZipList events
        --
        wholeboxgeometries :: ZipList (Diagram B)
        wholeboxgeometries = boxGeometry <$> wholestarts <*> wholestops
        partboxgeometries :: ZipList (Diagram B)
        partboxgeometries = boxGeometry <$> partstarts <*> partstops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> partstarts
        wholedesignatorgeometries :: ZipList (Diagram B)
        wholedesignatorgeometries = wholeDesignatorGeometry <$> ZipList events
        partdesignatorgeometries :: ZipList (Diagram B)
        partdesignatorgeometries = partDesignatorGeometry <$> partstarts
        --
        wholestyles :: ZipList (Diagram B -> Diagram B)
        wholestyles = (lc . d3Colors2 Dark) <$> colours
        partstyles :: ZipList (Diagram B -> Diagram B)
        partstyles = (pure $ lw none) <*> (fc . d3Colors2 Dark) <$> colours
        labelstyles :: ZipList (Diagram B -> Diagram B)
        labelstyles = (fc . d3Colors2 Light) <$> colours
        wholedesignatorstyles :: ZipList (Diagram B -> Diagram B)
        wholedesignatorstyles = (fc . d3Colors2 Light) <$> colours
        partdesignatorstyles :: ZipList (Diagram B -> Diagram B)
        partdesignatorstyles = (fc . d3Colors2 Light) <$> colours

