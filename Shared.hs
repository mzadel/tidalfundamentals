
module Shared where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.ColorSet (Brightness,d3Colors2)
import Data.Ratio
import qualified Data.Map as M (toList)
import qualified Sound.Tidal.Context as T

ratioToString :: Rational -> String
ratioToString r = case (denominator r) of
    1 -> show $ numerator r
    _ -> (show $ numerator r) ++ "/" ++ (show $ denominator r)

tickMarkLabelSize :: Measure Double
tickMarkLabelSize = local 0.02

eventWidth :: Double
eventWidth = 0.035

eventLabelSize :: Measure Double
eventLabelSize = local 0.03

eventLabelInset :: Rational
eventLabelInset = 0.02

linearDiagramVerticalPadding :: Double
linearDiagramVerticalPadding = 0.01

tickMarkLocations :: Rational -> Rational -> [Rational]
tickMarkLocations tickDivision endTickLoc = [0,tickDivision..endTickLoc]

style :: Brightness -> Int -> Diagram B -> Diagram B
style brightness colourindex = lw none $ fc $ d3Colors2 brightness colourindex

curveValueAtTime :: T.Pattern a -> T.Time -> a
curveValueAtTime ctspattern t = T.eventValue $ head events
    where
        events = T.queryArc ctspattern (T.Arc t t)

_showValueMap :: (T.Value -> String) -> T.ValueMap -> String
_showValueMap showValueFunc vmap = finalstring
    where
        pairstrings = map (\(k, v) -> k ++ ": " ++ (showValueFunc v)) (M.toList vmap)
        finalstring = foldr1 (\a b -> a ++ ", " ++ b) pairstrings

showValueMap :: T.ValueMap -> String
showValueMap = _showValueMap show

