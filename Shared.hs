
module Shared where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Ratio
import qualified Data.Map as M (toList)
import GHC.Float (formatRealFloat, FFFormat(FFGeneric, FFFixed))
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

curveValueAtTime :: T.Pattern a -> T.Time -> a
curveValueAtTime ctspattern t = T.eventValue $ head events
    where
        events = T.queryArc ctspattern (T.Arc t t)

arcMidpoint :: T.Arc -> T.Time
arcMidpoint (T.Arc s e) = (s + e) / 2

-- show doubles to three decimal places, and avoid trailing zeroes
showDoubleTruncated :: Double -> String
showDoubleTruncated d = formatFloat $ roundToDecimalPlace d
    where
        factor = 1000.0 :: Double

        roundToDecimalPlace :: Double -> Double
        roundToDecimalPlace x = (fromInteger $ round (x * factor)) / factor

        formatFloat :: RealFloat a => a -> String
        formatFloat v
            | abs v < 0.1 = formatRealFloat FFFixed (Just 3) v
            | otherwise   = formatRealFloat FFGeneric Nothing v

_showValueMap :: (T.Value -> String) -> T.ValueMap -> String
_showValueMap showValueFunc vmap = finalstring
    where
        pairstrings = map (\(k, v) -> k ++ ": " ++ (showValueFunc v)) (M.toList vmap)
        finalstring = foldr1 (\a b -> a ++ ", " ++ b) pairstrings

showValueMap :: T.ValueMap -> String
showValueMap = _showValueMap show

showValueMapTruncatedDouble :: T.ValueMap -> String
showValueMapTruncatedDouble = _showValueMap showValue
    where
        showValue :: T.Value -> String
        showValue (T.VF dub) = showDoubleTruncated dub
        showValue v = show v

lineOfText :: String -> Diagram B
lineOfText texttoshow = (alignedText 0 0.5 texttoshow # fontSize eventLabelSize) <> (strut (0.0 ^& 0.03))

