{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module TidalPatternDiagram where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow
import Data.Colour.Palette.ColorSet
import Data.Ratio
import Data.Map (Map, (!), toList)
import qualified Sound.Tidal.Context as T

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
ratioToString 0 = "0"
ratioToString r = (show $ numerator r) ++ "/" ++ (show $ denominator r)

tickMarkLabelSize = local 0.015

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

patternEventLabel :: String -> Rational -> Int -> Diagram B
patternEventLabel labelString wedgeStartLoc eventColour = labelDiagram
    where
        labelPos = p2 (theRadius, 0) # rotateBy ((fromRational wedgeStartLoc) + 0.02) # transform overallTransform
        labelDiagram = text labelString # fontSize eventLabelSize # fc (d3Colors2 Light eventColour) # moveTo labelPos

cycleDirectionArrow :: Diagram B
cycleDirectionArrow = arro
    where
        shaft = arc' (theRadius * 1.45) xDir (0.08 @@ turn) # transform overallTransform
        arro = arrowFromLocatedTrail shaft

tickMarkLocations :: Integer -> [Rational]
tickMarkLocations numTicks = map (% numTicks) [0..(numTicks-1)]

stripFirstAndLast :: String -> String
stripFirstAndLast = init . tail

eventToTriple :: T.Event T.ValueMap -> (String,Rational,Rational)
eventToTriple (T.Event _ _ (T.Arc start end) valueMap) = (stripFirstAndLast $ show value, start, end)
    where
        (_, value) = head $ toList valueMap

tidalPatternToEventList :: T.ControlPattern -> [(String,Rational,Rational)]
tidalPatternToEventList pat = eventList
    where
        queryResult = T.queryArc pat (T.Arc 0 1)
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
        events = tidalPatternToEventList tidalPattern
        patternevents = [patternEvent start end (colourTable ! label) | (label,start,end) <- events]
        patterneventlabels = [patternEventLabel label start (colourTable ! label) | (label,start,_) <- events]
        tickLocList = tickMarkLocations numTicks

patternEventLinear :: Rational -> Rational -> Diagram B
patternEventLinear startLoc endLoc = rect (fromRational $ endLoc-startLoc) eventWidth # alignL # moveTo ((fromRational $ startLoc) ^& 0)

patternEventLabelLinear :: String -> Rational -> Diagram B
patternEventLabelLinear labelString slabStartLoc = label
    where
        label = text labelString # fontSize eventLabelSize # moveTo labelPoint
        labelPoint = (fromRational (slabStartLoc + 0.02)) ^& 0

patternDiagramLinear :: T.ControlPattern -> Diagram B
patternDiagramLinear tidalPattern =
        mconcat patterneventlabels
        <> mconcat patternevents
    where
        events = tidalPatternToEventList tidalPattern
        patternevents = [patternEventLinear start end | (label,start,end) <- events]
        patterneventlabels = [patternEventLabelLinear label start | (label,start,_) <- events]

