
module LinearDiagrams.Curve (curveDiagram,curveDiagramLabeledPoint) where

import Shared
import LinearDiagrams.Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T
import Data.Ratio
import Control.Applicative (ZipList(ZipList,getZipList))

curveDiagramHeight :: Double
curveDiagramHeight = 0.2

curveDiagramTickMarkOffset :: Double
curveDiagramTickMarkOffset = fromRational (-eventLabelInset)

curveGeometry :: T.Pattern Double -> Diagram B
curveGeometry ctspattern = fromVertices (getZipList curvepoints)
    where
        curvepoints :: ZipList (P2 Double)
        curvepoints = (^&) <$> xs <*> ys
        xs :: ZipList Double
        xs = fromRational <$> ts
        ys :: ZipList Double
        ys = (curveValueAtTime ctspattern) <$> ts
        ts :: ZipList T.Time
        ts = ZipList [0, deltat .. 1]
        deltat = 1 % 100

curveDiagram :: T.Pattern Double -> Integer -> Diagram B
curveDiagram ctsPattern ticksPerCycle =
    curveandaxes # scaleY curveDiagramHeight
    <> mconcat (map tickMark tickLocList)
    <> mconcat (map tickMarkLabel tickLocList) # translateY curveDiagramTickMarkOffset
    where
        curveandaxes =
            fromOffsets [unitY]
            <> fromOffsets [unitX]
            <> curveGeometry ctsPattern
        tickLocList = tickMarkLocations (1%ticksPerCycle) 1

curveDiagramLabeledPoint :: (Double -> String) -> T.Pattern Double -> T.Time -> Diagram B
curveDiagramLabeledPoint formatLabel ctsPattern time = (thedot <> label) # translate (scaledpos .-. origin)
    where
        pos = (fromRational time) ^& curveValueAtTime ctsPattern time
        scaledpos = pos # scaleY curveDiagramHeight
        thedot = circle 0.0075 # fc red # lw none
        labelText = formatLabel $ curveValueAtTime ctsPattern time
        label = alignedText 0 0.5 labelText # fontSize eventLabelSize # translateX (fromRational eventLabelInset)

