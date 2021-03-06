
module LinearDiagrams.LinearDiagrams (diagramShowValue,diagramWithLanesShowValue,diagramShowCharValue,diagramWithLanesShowChar,diagramFromWholes,diagramWithLanesFromWholes) where

import Shared
import LinearDiagrams.Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.ColorSet (Brightness(Light,Dark),d3Colors2)
import qualified Sound.Tidal.Context as T
import Data.Ratio
import Control.Applicative (ZipList(ZipList,getZipList))

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
        boxStyles = (lw none . fc . d3Colors2 Dark) <$> colours
        labelStyles :: ZipList (Diagram B -> Diagram B)
        labelStyles = (lw none . fc . d3Colors2 Light) <$> colours
        laneTranslations :: ZipList (Diagram B -> Diagram B)
        laneTranslations = moveToLane <$> lanes

diagramShowCharValue :: T.Pattern Char -> Integer -> Rational -> (T.Event Char -> Int) -> Diagram B
diagramShowCharValue tidalPattern ticksPerCycle queryEnd colourFunc = diagramWithLanes showFunction tidalPattern ticksPerCycle queryEnd laneFunc colourFunc
    where
        showFunction = charToString . T.eventValue
        laneFunc _ = 0

-- lanes are numbered from zero, starting at the top
moveToLane :: Int -> Diagram B -> Diagram B
moveToLane lane = translateY ((fromIntegral $ -lane) * eventWidth)

diagramWithLanesShowChar :: T.Pattern Char -> Integer -> Rational -> (T.Event Char -> Int) -> (T.Event Char -> Int) -> Diagram B
diagramWithLanesShowChar = diagramWithLanes (charToString . T.eventValue)

diagramFromWholes :: (a -> String) -> T.Pattern a -> Rational -> Diagram B
diagramFromWholes formatLabel tidalPattern queryEnd = diagramWithLanesFromWholes formatLabel tidalPattern queryEnd laneFunc
    where
        laneFunc = const 0

diagramWithLanesFromWholes :: (a -> String) -> T.Pattern a -> Rational -> (T.Event a -> Int) -> Diagram B
diagramWithLanesFromWholes formatLabel tidalPattern queryEnd laneFunc =
    mconcat patterneventlabels <> mconcat patternevents
    where
        events = T.queryArc tidalPattern (T.Arc 0 queryEnd)
        eventswithonsets = ZipList $ filter T.eventHasOnset events
        --
        patternevents = getZipList $ laneTranslations <*> boxgeometries
        patterneventlabels = getZipList $ laneTranslations <*> labelgeometries
        --
        getLabel = formatLabel . T.eventValue
        --
        labels = getLabel <$> eventswithonsets
        lanes = laneFunc <$> eventswithonsets
        starts = T.wholeStart <$> eventswithonsets
        stops = T.wholeStop <$> eventswithonsets
        --
        boxgeometries :: ZipList (Diagram B)
        boxgeometries = boxGeometry <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> starts
        laneTranslations :: ZipList (Diagram B -> Diagram B)
        laneTranslations = moveToLane <$> lanes

