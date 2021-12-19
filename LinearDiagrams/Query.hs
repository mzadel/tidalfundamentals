
module LinearDiagrams.Query (queryDiagramChar) where

import Shared
import LinearDiagrams.Shared
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.ColorSet (Brightness(Light,Dark),d3Colors2)
import qualified Sound.Tidal.Context as T
import Control.Applicative (ZipList(ZipList,getZipList))

designatorSize :: Measure Double
designatorSize = eventLabelSize * 0.75

designatorInset :: Rational
designatorInset = eventLabelInset / 4

designatorVerticalOffset :: Double
designatorVerticalOffset = -eventWidth / 2

queryResultPartDrawing :: (T.Event a -> Int) -> T.Event a -> Diagram B
queryResultPartDrawing colourFunc e
    | not iszerowidth = queryResultPartDrawingNormal
    | otherwise = queryResultPartDrawingZW
    where
        iszerowidth = T.eventPartStart e == T.eventPartStop e
        colourindex = colourFunc e
        --
        queryResultPartDrawingNormal :: Diagram B
        queryResultPartDrawingNormal = boxdrawing <> designatordrawing
            where
                boxdrawing :: Diagram B
                boxdrawing = boxGeometry (T.eventPartStart e) (T.eventPartStop e) # lw none # fc (d3Colors2 Dark colourindex)
                --
                designatordrawing :: Diagram B
                designatordrawing = label # moveTo labelPoint # fc (d3Colors2 Light colourindex)
                    where
                        label = topLeftText "part" # fontSize designatorSize
                        labelPoint = (fromRational $ (T.eventPartStart e) + designatorInset) ^& designatorVerticalOffset
        --
        queryResultPartDrawingZW :: Diagram B
        queryResultPartDrawingZW  = boxdrawing <> designatordrawing
            where
                boxdrawing :: Diagram B
                boxdrawing = boxGeometry (T.eventPartStart e) (T.eventPartStop e) # lc (d3Colors2 Dark colourindex)
                --
                designatordrawing :: Diagram B
                designatordrawing = label # moveTo labelPoint # fc (d3Colors2 Light colourindex)
                    where
                        label = alignedText 0.5 1 "zero-width part" # fontSize designatorSize
                        labelPoint = (fromRational $ (T.eventPartStart e)) ^& designatorVerticalOffset

queryResultWholeDrawing :: (T.Event a -> Int) -> T.Event a -> Diagram B
queryResultWholeDrawing _ (T.Event _ Nothing _ _) = mempty
queryResultWholeDrawing colourFunc e@(T.Event _ (Just thewhole) thepart _) = boxdrawing <> designatordrawing
    where
        colourindex = colourFunc e
        --
        boxdrawing :: Diagram B
        boxdrawing = boxGeometry (T.start thewhole) (T.stop thewhole) # lc (d3Colors2 Dark colourindex)
        --
        designatordrawing :: Diagram B
        designatordrawing = designatorgeometry # fc (d3Colors2 Light colourindex)
        --
        designatorgeometry :: Diagram B
        designatorgeometry
            | (T.start thewhole < T.start thepart) = wholeDesignatorGeometryAtLeft
            | (T.stop thepart < T.stop thewhole) = wholeDesignatorGeometryAtRight
            -- otherwise thewhole == thepart
            | otherwise = wholeDesignatorGeometryAtRight
        --
        wholeDesignatorGeometryAtLeft :: Diagram B
        wholeDesignatorGeometryAtLeft = label # moveTo labelPoint
            where
                label = topLeftText "whole" # fontSize designatorSize
                labelPoint = (fromRational $ (T.wholeStart e) + designatorInset) ^& designatorVerticalOffset
        --
        wholeDesignatorGeometryAtRight :: Diagram B
        wholeDesignatorGeometryAtRight = label # moveTo labelPoint
            where
                label = alignedText 1 1 "whole" # fontSize designatorSize
                labelPoint = (fromRational $ (T.wholeStop e) - designatorInset) ^& designatorVerticalOffset

queryDiagram :: (T.Event a -> String) -> [T.Event a] -> (T.Event a -> Int) -> Diagram B
queryDiagram formatValue events colourFunc =
    mconcat valuelabels
    <> mconcat partdrawings
    <> mconcat wholedrawings
    where
        wholedrawings = fmap (queryResultWholeDrawing colourFunc) events
        partdrawings = fmap (queryResultPartDrawing colourFunc) events
        valuelabels = getZipList $ labelstyles <*> labelgeometries
        --
        labels = formatValue <$> ZipList events
        partstarts = T.eventPartStart <$> ZipList events
        colours = colourFunc <$> ZipList events
        --
        labelgeometries :: ZipList (Diagram B)
        labelgeometries = labelGeometry <$> labels <*> partstarts
        --
        labelstyles :: ZipList (Diagram B -> Diagram B)
        labelstyles = (fc . d3Colors2 Light) <$> colours

queryDiagramChar :: [T.Event Char] -> (T.Event Char -> Int) -> Diagram B
queryDiagramChar = queryDiagram (charToString . T.eventValue)

