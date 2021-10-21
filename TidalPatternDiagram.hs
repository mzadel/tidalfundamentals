{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module TidalPatternDiagram where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow
import Data.Colour.Palette.ColorSet
import Data.Ratio

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

tickMarkLabel :: Double -> Rational -> Diagram B
tickMarkLabel extraRadius tickLoc = label
    where
        labelStartPoint = p2 (theRadius + extraRadius, 0)
        rotAmount = fromRational tickLoc
        labelPoint = labelStartPoint # rotateBy rotAmount # transform overallTransform
        labelText = ratioToString tickLoc
        label = text labelText # fontSize (local 0.015) # moveTo labelPoint

patternEvent :: Rational -> Rational -> Diagram B
patternEvent startLoc endLoc = transformedWedge
    where
        angle = (fromRational (endLoc - startLoc - 0.003)) @@ turn
        startDir = xDir # rotateBy (fromRational startLoc)
        wedgeWidth = 0.035
        innerRadius = theRadius - (wedgeWidth/2)
        outerRadius = theRadius + (wedgeWidth/2)
        theWedge = annularWedge outerRadius innerRadius startDir angle # fc (d3Colors2 Dark eventColour) # lw none
        transformedWedge = theWedge # transform overallTransform

patternEventLabel :: String -> Rational -> Diagram B
patternEventLabel labelString wedgeStartLoc = labelDiagram
    where
        labelPos = p2 (theRadius, 0) # rotateBy ((fromRational wedgeStartLoc) + 0.02) # transform overallTransform
        labelDiagram = text labelString # fontSize (local 0.03) # fc (d3Colors2 Light eventColour) # moveTo labelPos

cycleDirectionArrow :: Diagram B
cycleDirectionArrow = arro
    where
        shaft = arc' (theRadius * 1.45) xDir (0.08 @@ turn) # transform overallTransform
        arro = arrowFromLocatedTrail shaft

numParts = 7
tickMarkLocations = map (/ numParts) [0..(numParts-1)] :: [Rational]

eventColour = 2

combined = patternEventLabel "a" (1/7)
    <> patternEvent (1/7) (2/7)
    <> patternEventLabel "b" (3/7)
    <> patternEvent (3/7) (5/7)
    <> cycleDirectionArrow
    <> mconcat (map tickMark tickMarkLocations)
    <> mconcat (map (tickMarkLabel 0.05) tickMarkLocations)
    <> circle theRadius

outputScaling = 1000 :: Double

outputDiagram = combined # frame 0.05 # scale outputScaling

