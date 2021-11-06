
module CircularDiagrams (diagram) where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow
import Data.Colour.Palette.ColorSet
import qualified Sound.Tidal.Context as T
import Data.Ratio
import Data.Map (Map, (!))
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

tickMarkLabel :: Double -> Rational -> Diagram B
tickMarkLabel extraRadius tickLoc = label
    where
        labelStartPoint = p2 (theRadius + extraRadius, 0)
        rotAmount = fromRational tickLoc
        labelPoint = labelStartPoint # rotateBy rotAmount # transform overallTransform
        labelText = ratioToString tickLoc
        label = text labelText # fontSize tickMarkLabelSize # moveTo labelPoint

wedgeGeometry :: Rational -> Rational -> Diagram B
wedgeGeometry startLoc endLoc = transformedWedge
    where
        angle = (fromRational (endLoc - startLoc - 0.003)) @@ turn
        startDir = xDir # rotateBy (fromRational startLoc)
        innerRadius = theRadius - (eventWidth/2)
        outerRadius = theRadius + (eventWidth/2)
        theWedge = annularWedge outerRadius innerRadius startDir angle
        transformedWedge = theWedge # transform overallTransform

labelGeometry :: String -> Rational -> Diagram B
labelGeometry labelString wedgeStartLoc = labelDiagram
    where
        labelPos = p2 (theRadius, 0) # rotateBy (fromRational (wedgeStartLoc + eventLabelInset)) # transform overallTransform
        labelDiagram = text labelString # fontSize eventLabelSize # moveTo labelPos

cycleDirectionArrow :: Diagram B
cycleDirectionArrow = arro
    where
        shaft = arc' (theRadius * 1.45) xDir (0.08 @@ turn) # transform overallTransform
        arro = arrowFromLocatedTrail shaft

tickMarkLabelOffset = 0.05

diagram :: T.ControlPattern -> Integer -> Map String Int -> Diagram B
diagram tidalPattern numTicks colourTable =
        mconcat patterneventlabels
        <> mconcat patternevents
        <> cycleDirectionArrow
        <> mconcat (map tickMark tickLocList)
        <> mconcat (map (tickMarkLabel tickMarkLabelOffset) tickLocList)
        <> circle theRadius
    where
        events = ZipList $ T.queryArc tidalPattern (T.Arc 0 1)
        --
        patternevents = getZipList $ wedgeStyles <*> wedgegeometries
        patterneventlabels = getZipList $ labelStyles <*> labelgeometries
        --
        tickLocList = init $ tickMarkLocations (1%numTicks) 1
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
        wedgegeometries :: ZipList (Diagram B)
        wedgegeometries = wedgeGeometry <$> starts <*> stops
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> starts
        wedgeStyles :: ZipList (Diagram B -> Diagram B)
        wedgeStyles = style Dark <$> colours
        labelStyles :: ZipList (Diagram B -> Diagram B)
        labelStyles = style Light <$> colours
