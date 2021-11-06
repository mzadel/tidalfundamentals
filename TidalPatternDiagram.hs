{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module TidalPatternDiagram (patternDiagram) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow
import Data.Colour.Palette.ColorSet
import Data.Ratio
import Data.Map (Map, (!), toList)
import qualified Sound.Tidal.Context as T
import Shared

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

tickMarkLabel :: Double -> Rational -> Diagram B
tickMarkLabel extraRadius tickLoc = label
    where
        labelStartPoint = p2 (theRadius + extraRadius, 0)
        rotAmount = fromRational tickLoc
        labelPoint = labelStartPoint # rotateBy rotAmount # transform overallTransform
        labelText = ratioToString tickLoc
        label = text labelText # fontSize tickMarkLabelSize # moveTo labelPoint

patternEvent :: Rational -> Rational -> Int -> Diagram B
patternEvent startLoc endLoc eventColour = transformedWedge
    where
        angle = (fromRational (endLoc - startLoc - 0.003)) @@ turn
        startDir = xDir # rotateBy (fromRational startLoc)
        innerRadius = theRadius - (eventWidth/2)
        outerRadius = theRadius + (eventWidth/2)
        theWedge = annularWedge outerRadius innerRadius startDir angle # fc (d3Colors2 Dark eventColour) # lw none
        transformedWedge = theWedge # transform overallTransform

patternEventLabel :: String -> Rational -> Int -> Diagram B
patternEventLabel labelString wedgeStartLoc eventColour = labelDiagram
    where
        labelPos = p2 (theRadius, 0) # rotateBy (fromRational (wedgeStartLoc + eventLabelInset)) # transform overallTransform
        labelDiagram = text labelString # fontSize eventLabelSize # fc (d3Colors2 Light eventColour) # moveTo labelPos

cycleDirectionArrow :: Diagram B
cycleDirectionArrow = arro
    where
        shaft = arc' (theRadius * 1.45) xDir (0.08 @@ turn) # transform overallTransform
        arro = arrowFromLocatedTrail shaft

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

