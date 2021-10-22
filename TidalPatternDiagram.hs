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
ratioToString r = case (denominator r) of
    1 -> show $ numerator r
    _ -> (show $ numerator r) ++ "/" ++ (show $ denominator r)

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
eventLabelInset = 0.02

patternEventLabel :: String -> Rational -> Int -> Diagram B
patternEventLabel labelString wedgeStartLoc eventColour = labelDiagram
    where
        labelPos = p2 (theRadius, 0) # rotateBy ((fromRational wedgeStartLoc) + eventLabelInset) # transform overallTransform
        labelDiagram = text labelString # fontSize eventLabelSize # fc (d3Colors2 Light eventColour) # moveTo labelPos

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

patternEventLinear :: Rational -> Rational -> Int -> Diagram B
patternEventLinear startLoc endLoc eventColour = rect (fromRational $ endLoc-startLoc) eventWidth # alignL # fc (d3Colors2 Dark eventColour) # lw none # moveTo ((fromRational $ startLoc) ^& 0)

patternEventLabelLinear :: String -> Rational -> Int -> Diagram B
patternEventLabelLinear labelString slabStartLoc eventColour = label
    where
        label = text labelString # fontSize eventLabelSize # fc (d3Colors2 Light eventColour) # moveTo labelPoint
        labelPoint = (fromRational (slabStartLoc + eventLabelInset)) ^& 0

linearDiagramVerticalPadding = 0.01

patternDiagramLinear :: T.ControlPattern -> Integer -> Map String Int -> Diagram B
patternDiagramLinear tidalPattern numTicks colourTable =
        vsep linearDiagramVerticalPadding [
            mconcat (map tickMarkLabelLinear tickLocList)
            ,mconcat (map tickMarkLinear tickLocList)
            ,(mconcat patterneventlabels <> mconcat patternevents)
            ]
    where
        events = tidalPatternToEventList tidalPattern
        patternevents = [patternEventLinear start end (colourTable ! label) | (label,start,end) <- events]
        patterneventlabels = [patternEventLabelLinear label start (colourTable ! label) | (label,start,_) <- events]
        tickLocList = tickMarkLocations (1%numTicks) 1

