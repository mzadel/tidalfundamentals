
module LinearDiagrams.Arc (arcDiagram) where

import Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Sound.Tidal.Context as T
import Control.Applicative (ZipList(ZipList,getZipList))

arcGeometry :: T.Arc -> Diagram B
arcGeometry (T.Arc startLoc stopLoc)
    | startLoc /= stopLoc = (strokeP leftedge <> strokeP rightedge <> thearrow) # alignL # moveTo ((fromRational startLoc) ^& 0)
    -- otherwise startLoc == stopLoc
    | otherwise = strokeP leftedge # moveTo ((fromRational startLoc) ^& 0)
        where
            ysize = eventWidth
            xsize = fromRational $ stopLoc - startLoc
            leftedge = vrule ysize
            rightedge = vrule ysize # translateX xsize
            thearrow = arrowV' arrowopts (xsize ^& 0) # translateY (-ysize * 0.35)
            arrowopts = with
                & arrowHead .~ dart & headLength .~ small
                & arrowTail .~ dart' & tailLength .~ small

arcLabelGeometry :: String -> T.Arc -> Diagram B
arcLabelGeometry labelString (T.Arc arcStartLoc arcStopLoc) = label
    where
        label = alignedText xalignment 0.5 labelString # fontSize eventLabelSize # moveTo labelPos
        labelPos = (fromRational (arcStartLoc + arcStopLoc) / 2) ^& 0
        xalignment
            -- centre text in the typical case
            | arcStartLoc /= arcStopLoc = 0.5
            -- left-align text in the degenerate case (arcStartLoc == arcStopLoc)
            | otherwise = 0.0

arcDiagram :: [T.Arc] -> Diagram B
arcDiagram arcs =
    mconcat arclabels <> mconcat arcdrawings
    where
        arcdrawings = getZipList $ arcgeometries
        arclabels = getZipList $ labelgeometries
        --
        getLabel :: T.Arc -> String
        getLabel a = "Arc " ++ (show $ startdouble) ++ " " ++ (show $ stopdouble)
            where
                startdouble = fromRational $ T.start a :: Double
                stopdouble = fromRational $ T.stop a :: Double
        --
        zarcs = ZipList arcs
        labels = getLabel <$> zarcs
        --
        arcgeometries :: ZipList (Diagram B)
        arcgeometries = arcGeometry <$> zarcs
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = arcLabelGeometry <$> labels <*> zarcs

