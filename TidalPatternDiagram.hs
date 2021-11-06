{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module TidalPatternDiagram (patternDiagram,patternDiagramLinear,patternDiagramLinearWithLanes,patternDiagramLinearWithDoubles,patternDiagramLinearWithValueMaps,linearDiagramVerticalPadding,eventLabelSize) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow
import Data.Colour.Palette.ColorSet
import Data.Ratio
import Data.Map (Map, (!), toList)
import qualified Sound.Tidal.Context as T
import Control.Applicative (ZipList(ZipList,getZipList))

radiusOfUnitCircumfrenceCircle = 1.0 / (2.0 * pi) :: Double
theRadius = radiusOfUnitCircumfrenceCircle

{--
Verify that radiusOfUnitCircumfrenceCircle gives us a circle of circumfrence
1.0:

> c = circle radiusOfUnitCircumfrenceCircle :: Trail V2 Double
> stdArcLength c
1.0001402927134473
--}

overallTransform :: Transformation V2 Double
overallTransform = scalingY (-1) <> (rotation $ (-1/4) @@ turn)

tickMark :: Rational -> Diagram B
tickMark tickLoc = mark
    where
        tickMarkSize = 0.1 * theRadius
        startPoint = p2 (theRadius, 0)
        rotAmount = fromRational tickLoc
        mark = hrule tickMarkSize # moveTo startPoint # rotateBy rotAmount # transform overallTransform

ratioToString :: Rational -> String
ratioToString r = case (denominator r) of
    1 -> show $ numerator r
    _ -> (show $ numerator r) ++ "/" ++ (show $ denominator r)

tickMarkLabelSize = local 0.02

tickMarkLabel :: Double -> Rational -> Diagram B
tickMarkLabel extraRadius tickLoc = label
    where
        labelStartPoint = p2 (theRadius + extraRadius, 0)
        rotAmount = fromRational tickLoc
        labelPoint = labelStartPoint # rotateBy rotAmount # transform overallTransform
        labelText = ratioToString tickLoc
        label = text labelText # fontSize tickMarkLabelSize # moveTo labelPoint

eventWidth = 0.035

patternEvent :: Rational -> Rational -> Int -> Diagram B
patternEvent startLoc endLoc eventColour = transformedWedge
    where
        angle = (fromRational (endLoc - startLoc - 0.003)) @@ turn
        startDir = xDir # rotateBy (fromRational startLoc)
        innerRadius = theRadius - (eventWidth/2)
        outerRadius = theRadius + (eventWidth/2)
        theWedge = annularWedge outerRadius innerRadius startDir angle # fc (d3Colors2 Dark eventColour) # lw none
        transformedWedge = theWedge # transform overallTransform

eventLabelSize = local 0.03
eventLabelInset = 0.02

patternEventLabel :: String -> Rational -> Int -> Diagram B
patternEventLabel labelString wedgeStartLoc eventColour = labelDiagram
    where
        labelPos = p2 (theRadius, 0) # rotateBy ((fromRational wedgeStartLoc) + eventLabelInset) # transform overallTransform
        labelDiagram = text labelString # fontSize eventLabelSize # fc (d3Colors2 Light eventColour) # moveTo labelPos

styleX :: Brightness -> Int -> Diagram B -> Diagram B
styleX brightness colourindex = lw none $ fc $ d3Colors2 brightness colourindex

cycleDirectionArrow :: Diagram B
cycleDirectionArrow = arro
    where
        shaft = arc' (theRadius * 1.45) xDir (0.08 @@ turn) # transform overallTransform
        arro = arrowFromLocatedTrail shaft

tickMarkLocations :: Rational -> Rational -> [Rational]
tickMarkLocations tickDivision endTickLoc = [0,tickDivision..endTickLoc]

stripFirstAndLast :: String -> String
stripFirstAndLast = init . tail

eventToTriple :: T.Event T.ValueMap -> (String,Rational,Rational)
eventToTriple (T.Event _ _ (T.Arc start end) valueMap) = (stripFirstAndLast $ show value, start, end)
    where
        (_, value) = head $ toList valueMap

tidalPatternToEventList :: T.ControlPattern -> Rational -> [(String,Rational,Rational)]
tidalPatternToEventList pat queryEnd = eventList
    where
        queryResult = T.queryArc pat (T.Arc 0 queryEnd)
        eventList = map eventToTriple queryResult

tickMarkLabelOffset = 0.05

patternDiagram :: T.ControlPattern -> Integer -> Map String Int -> Diagram B
patternDiagram tidalPattern numTicks colourTable =
        mconcat patterneventlabels
        <> mconcat patternevents
        <> cycleDirectionArrow
        <> mconcat (map tickMark tickLocList)
        <> mconcat (map (tickMarkLabel tickMarkLabelOffset) tickLocList)
        <> circle theRadius
    where
        events = tidalPatternToEventList tidalPattern 1
        patternevents = [patternEvent start end (colourTable ! label) | (label,start,end) <- events]
        patterneventlabels = [patternEventLabel label start (colourTable ! label) | (label,start,_) <- events]
        tickLocList = init $ tickMarkLocations (1%numTicks) 1

tickMarkLinear :: Rational -> Diagram B
tickMarkLinear tickLoc = mark
    where
        tickMarkSize = 0.1 * theRadius
        point = p2 (fromRational tickLoc, 0)
        mark = vrule tickMarkSize # moveTo point

tickMarkLabelLinear :: Rational -> Diagram B
tickMarkLabelLinear tickLoc = label
    where
        labelPoint = p2 (fromRational tickLoc, 0)
        labelText = ratioToString tickLoc
        label = text labelText # fontSize tickMarkLabelSize # alignB # moveTo labelPoint

patternEventLinearX :: Rational -> Rational -> Diagram B
patternEventLinearX startLoc endLoc = rect (fromRational $ endLoc-startLoc) eventWidth # alignL # moveTo ((fromRational $ startLoc) ^& 0)

patternEventLabelLinearX :: String -> Rational -> Diagram B
patternEventLabelLinearX labelString slabStartLoc = label
    where
        label = alignedText 0 0.5 labelString # fontSize eventLabelSize # moveTo labelPoint
        labelPoint = (fromRational (slabStartLoc + eventLabelInset)) ^& 0

linearDiagramVerticalPadding = 0.01

patternDiagramLinear :: T.ControlPattern -> Integer -> Rational -> Map String Int -> Diagram B
patternDiagramLinear tidalPattern ticksPerCycle queryEnd colourTable =
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
        boxgeometries = patternEventLinearX <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = patternEventLabelLinearX <$> labels <*> starts
        labelStyles :: ZipList (Diagram B -> Diagram B)
        labelStyles = styleX Light <$> colours
        boxStyles :: ZipList (Diagram B -> Diagram B)
        boxStyles = styleX Dark <$> colours

-- lanes are numbered from zero, starting at the top
moveToLaneX :: Int -> Diagram B -> Diagram B
moveToLaneX lane = translateY ((fromIntegral $ -lane) * eventWidth)

patternDiagramLinearWithLanes :: T.ControlPattern -> Integer -> Rational -> Map String Int -> Map String Int -> Diagram B
patternDiagramLinearWithLanes tidalPattern ticksPerCycle queryEnd laneTable colourTable =
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
        boxgeometries = patternEventLinearX <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = patternEventLabelLinearX <$> labels <*> starts
        boxStyles :: ZipList (Diagram B -> Diagram B)
        boxStyles = styleX Dark <$> colours
        labelStyles :: ZipList (Diagram B -> Diagram B)
        labelStyles = styleX Light <$> colours
        laneTranslations :: ZipList (Diagram B -> Diagram B)
        laneTranslations = moveToLaneX <$> lanes

patternDiagramLinearWithDoubles :: T.Pattern Double -> Rational -> Diagram B
patternDiagramLinearWithDoubles tidalPattern queryEnd =
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
        boxgeometries = patternEventLinearX <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = patternEventLabelLinearX <$> labels <*> starts

prettyPrintValueMap :: T.ValueMap -> String
prettyPrintValueMap vmap = finalstring
    where
        pairstrings = map (\(k, v) -> k ++ ": " ++ (show v)) (toList vmap)
        finalstring = foldr1 (\a b -> a ++ ", " ++ b) pairstrings

patternDiagramLinearWithValueMaps :: T.Pattern T.ValueMap -> Rational -> Diagram B
patternDiagramLinearWithValueMaps tidalPattern queryEnd =
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
        boxgeometries = patternEventLinearX <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = patternEventLabelLinearX <$> labels <*> starts

