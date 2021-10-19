{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

radiusOfUnitCircumfrenceCircle = 1.0 / (2.0 * pi) :: Double
theRadius = radiusOfUnitCircumfrenceCircle

{--
Verify that radiusOfUnitCircumfrenceCircle gives us a circle of circumfrence
1.0:

> c = circle radiusOfUnitCircumfrenceCircle :: Trail V2 Double
> stdArcLength c
1.0001402927134473
--}

circleWithTicks :: Diagram B
circleWithTicks = circle theRadius <> mconcat tickMarks <> mconcat tickMarkLabels
    where
        numParts = 7
        tickMarkLocations = map (/ numParts) [0..(numParts-1)] :: [Rational]
        tickMarks = map tickMark tickMarkLocations
        tickMarkLabels = map (tickMarkLabel 0.05) tickMarkLocations

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
        labelText = show tickLoc
        label = text labelText # fontSize (local 0.015) # moveTo labelPoint

patternEvent :: Rational -> Rational -> Diagram B
patternEvent startLoc endLoc = transformedWedge
    where
        angle = (fromRational (endLoc - startLoc - 0.003)) @@ turn
        startDir = xDir # rotateBy (fromRational startLoc)
        wedgeWidth = 0.035
        innerRadius = theRadius - (wedgeWidth/2)
        outerRadius = theRadius + (wedgeWidth/2)
        theWedge = annularWedge outerRadius innerRadius startDir angle # fc red # lw none
        transformedWedge = theWedge # transform overallTransform

patternEventLabel :: String -> Rational -> Diagram B
patternEventLabel labelString wedgeStartLoc = labelDiagram
    where
        labelPos = p2 (theRadius, 0) # rotateBy ((fromRational wedgeStartLoc) + 0.02) # transform overallTransform
        labelDiagram = text labelString # fontSize (local 0.03) # moveTo labelPos

patternEvents :: Diagram B
patternEvents = patternEventLabel "a" (1/7) <> patternEvent (1/7) (2/7)

combined = patternEvents <> circleWithTicks

outputScaling = 1000 :: Double

outputDiagram = combined

main :: IO ()
main = mainWith (outputDiagram # frame 0.05 # scale outputScaling)

