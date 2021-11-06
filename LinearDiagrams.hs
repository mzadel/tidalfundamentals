{-# LANGUAGE FlexibleInstances         #-}

module LinearDiagrams (diagramLabeledFromSValue,diagramWithLanesLabeledFromSValue,diagramFromWholes) where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.ColorSet (Brightness(Light,Dark))
import qualified Sound.Tidal.Context as T
import Data.Ratio
import qualified Data.Map as M (Map, (!), toList, findWithDefault, empty)
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

diagramLabeledFromSValue :: T.ControlPattern -> Integer -> Rational -> M.Map String Int -> Diagram B
diagramLabeledFromSValue tidalPattern ticksPerCycle queryEnd colourTable = diagramWithLanesLabeledFromSValue tidalPattern ticksPerCycle queryEnd laneTable colourTable
    where
        laneTable = M.empty

-- lanes are numbered from zero, starting at the top
moveToLane :: Int -> Diagram B -> Diagram B
moveToLane lane = translateY ((fromIntegral $ -lane) * eventWidth)

diagramWithLanesLabeledFromSValue :: T.ControlPattern -> Integer -> Rational -> M.Map String Int -> M.Map String Int -> Diagram B
diagramWithLanesLabeledFromSValue tidalPattern ticksPerCycle queryEnd laneTable colourTable =
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
        getLabel :: T.Event T.ValueMap -> String
        getLabel e = T.svalue $ T.eventValue e M.! "s"
        lookUpColour :: T.Event T.ValueMap -> Int
        lookUpColour e = colourTable M.! getLabel e
        lookUpLane :: T.Event T.ValueMap -> Int
        lookUpLane e = M.findWithDefault 0 (getLabel e) laneTable
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
        laneTranslations = moveToLane <$> lanes

class Labelable l where
    toLabel :: l -> String

instance Labelable Double where
    toLabel d = show d

instance Labelable T.ValueMap where
    toLabel vmap = finalstring
        where
            pairstrings = map (\(k, v) -> k ++ ": " ++ (show v)) (M.toList vmap)
            finalstring = foldr1 (\a b -> a ++ ", " ++ b) pairstrings

diagramFromWholes :: (Labelable a) => T.Pattern a -> Rational -> Diagram B
diagramFromWholes tidalPattern queryEnd =
    mconcat patterneventlabels <> mconcat patternevents
    where
        events = T.queryArc tidalPattern (T.Arc 0 queryEnd)
        eventswithonsets = ZipList $ filter T.eventHasOnset events
        --
        patternevents = getZipList $ boxgeometries
        patterneventlabels = getZipList $ labelgeometries
        --
        getLabel :: (Labelable a) => T.Event a -> String
        getLabel = toLabel . T.eventValue
        --
        labels = getLabel <$> eventswithonsets
        starts = T.wholeStart <$> eventswithonsets
        stops = T.wholeStop <$> eventswithonsets
        --
        boxgeometries :: ZipList (Diagram B)
        boxgeometries = boxGeometry <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> starts

