
module DiagramTable where

import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Map (Map,fromList,toList)
import qualified Sound.Tidal.Context as T

outputScaling = 1000 :: Double

colourTable :: Map String Int
colourTable = fromList [
     ("a", 0)
    ,("b", 2)
    ,("c", 1)
    ,("d", 7)
    ,("e", 3)
    ,("bd", 0)]

patternTable :: Map String (String, Integer)
patternTable = fromList [
    ("basicpattern", ("a b c", 3))
    ,("tildeisarest", ("bd ~ bd ~", 4))
    ,("underscoreelongates", ("a _ c", 3))
    ,("repeateventasterisk", ("a*2 c", 4))
    ,("repeateventbang", ("a!2 c", 3))
    ,("squarebrackets", ("[a b c] [d e]", 4))
    ,("thedot", ("a b c . d e", 4))
    ,("parallelpat1", ("a b c", 4))
    ,("parallelpat2", ("d e", 4))
    ]

diagramEntry :: (String, (String, Integer)) -> (String, Diagram B)
diagramEntry (label, (patString, numticks)) = (label, diagram)
    where
        diagram = patternDiagram pat numticks colourTable # frame 0.05 # scale outputScaling
        pat = T.s $ T.parseBP_E patString

diagramListForMainWith :: [(String, Diagram B)]
diagramListForMainWith = map diagramEntry $ toList patternTable

