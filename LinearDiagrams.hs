
module LinearDiagrams (diagramLabeledFromSValue,diagramWithLanesLabeledFromSValue,diagramWithDoubles,diagramWithValueMaps) where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.ColorSet
import qualified Sound.Tidal.Context as T
import Data.Ratio
import Data.Map (Map, (!), toList)
import Control.Applicative (ZipList(ZipList,getZipList))

tickMarkLinear :: Rational -> Diagram B
tickMarkLinear tickLoc = mark
    where
        tickMarkSize = 0.016
        point = p2 (fromRational tickLoc, 0)
        mark = vrule tickMarkSize # moveTo point

tickMarkLabelLinear :: Rational -> Diagram B
tickMarkLabelLinear tickLoc = label
    where
        labelPoint = p2 (fromRational tickLoc, 0)
        labelText = ratioToString tickLoc
        label = text labelText # fontSize tickMarkLabelSize # alignB # moveTo labelPoint

boxGeometry :: Rational -> Rational -> Diagram B
boxGeometry startLoc endLoc = rect (fromRational $ endLoc-startLoc) eventWidth # alignL # moveTo ((fromRational $ startLoc) ^& 0)

labelGeometry :: String -> Rational -> Diagram B
labelGeometry labelString slabStartLoc = label
    where
        label = alignedText 0 0.5 labelString # fontSize eventLabelSize # moveTo labelPoint
        labelPoint = (fromRational (slabStartLoc + eventLabelInset)) ^& 0

diagramLabeledFromSValue :: T.ControlPattern -> Integer -> Rational -> Map String Int -> Diagram B
diagramLabeledFromSValue tidalPattern ticksPerCycle queryEnd colourTable =
        vsep linearDiagramVerticalPadding [
            mconcat (map tickMarkLabelLinear tickLocList)
            ,mconcat (map tickMarkLinear tickLocList)
            ,(mconcat patterneventlabels <> mconcat patternevents)
            ]
    where
        events = ZipList $ T.queryArc tidalPattern (T.Arc 0 queryEnd)
        --
        patternevents = getZipList $ boxStyles <*> boxgeometries
        patterneventlabels = getZipList $ labelStyles <*> labelgeometries
        --
        tickLocList = tickMarkLocations (1%ticksPerCycle) queryEnd
        --
        getLabel :: T.Event T.ValueMap -> String
        getLabel e = T.svalue $ T.eventValue e ! "s"
        lookUpColour :: T.Event T.ValueMap -> Int
        lookUpColour e = colourTable ! getLabel e
        --
        labels = getLabel <$> events
        colours = lookUpColour <$> events
        starts = T.eventPartStart <$> events
        stops = T.eventPartStop <$> events
        --
        boxgeometries :: ZipList (Diagram B)
        boxgeometries = boxGeometry <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> starts
        labelStyles :: ZipList (Diagram B -> Diagram B)
        labelStyles = style Light <$> colours
        boxStyles :: ZipList (Diagram B -> Diagram B)
        boxStyles = style Dark <$> colours

-- lanes are numbered from zero, starting at the top
moveToLaneX :: Int -> Diagram B -> Diagram B
moveToLaneX lane = translateY ((fromIntegral $ -lane) * eventWidth)

diagramWithLanesLabeledFromSValue :: T.ControlPattern -> Integer -> Rational -> Map String Int -> Map String Int -> Diagram B
diagramWithLanesLabeledFromSValue tidalPattern ticksPerCycle queryEnd laneTable colourTable =
        vsep linearDiagramVerticalPadding [
            mconcat (map tickMarkLabelLinear tickLocList)
            ,mconcat (map tickMarkLinear tickLocList)
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
        getLabel :: T.Event T.ValueMap -> String
        getLabel e = T.svalue $ T.eventValue e ! "s"
        lookUpColour :: T.Event T.ValueMap -> Int
        lookUpColour e = colourTable ! getLabel e
        lookUpLane :: T.Event T.ValueMap -> Int
        lookUpLane e = laneTable ! getLabel e
        --
        labels = getLabel <$> events
        colours = lookUpColour <$> events
        lanes = lookUpLane <$> events
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
        laneTranslations = moveToLaneX <$> lanes

diagramWithDoubles :: T.Pattern Double -> Rational -> Diagram B
diagramWithDoubles tidalPattern queryEnd =
    mconcat patterneventlabels <> mconcat patternevents
    where
        events = T.queryArc tidalPattern (T.Arc 0 queryEnd)
        eventswithonsets = ZipList $ filter T.eventHasOnset events
        --
        patternevents = getZipList $ boxgeometries
        patterneventlabels = getZipList $ labelgeometries
        --
        getLabel :: T.Event Double -> String
        getLabel = show . T.eventValue
        --
        labels = getLabel <$> eventswithonsets
        starts = T.wholeStart <$> eventswithonsets
        stops = T.wholeStop <$> eventswithonsets
        --
        boxgeometries :: ZipList (Diagram B)
        boxgeometries = boxGeometry <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> starts

prettyPrintValueMap :: T.ValueMap -> String
prettyPrintValueMap vmap = finalstring
    where
        pairstrings = map (\(k, v) -> k ++ ": " ++ (show v)) (toList vmap)
        finalstring = foldr1 (\a b -> a ++ ", " ++ b) pairstrings

diagramWithValueMaps :: T.Pattern T.ValueMap -> Rational -> Diagram B
diagramWithValueMaps tidalPattern queryEnd =
    mconcat patterneventlabels <> mconcat patternevents
    where
        events = T.queryArc tidalPattern (T.Arc 0 queryEnd)
        eventswithonsets = ZipList $ filter T.eventHasOnset events
        --
        patternevents = getZipList $ boxgeometries
        patterneventlabels = getZipList $ labelgeometries
        --
        getLabel :: T.Event T.ValueMap -> String
        getLabel = prettyPrintValueMap . T.eventValue
        --
        labels = getLabel <$> eventswithonsets
        starts = T.wholeStart <$> eventswithonsets
        stops = T.wholeStop <$> eventswithonsets
        --
        boxgeometries :: ZipList (Diagram B)
        boxgeometries = boxGeometry <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> starts

