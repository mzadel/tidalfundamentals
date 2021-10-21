
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
    ,("b", 2)]

patternTable :: Map String (String, Integer)
patternTable = fromList [
    ("pat1", ("a a b", 3)),
    ("pat2", ("a b b", 3))
    ]

diagramEntry :: (String, (String, Integer)) -> (String, Diagram B)
diagramEntry (label, (patString, numticks)) = (label, diagram)
    where
        diagram = patternDiagram pat numticks colourTable # frame 0.05 # scale outputScaling
        pat = T.s $ T.parseBP_E patString

diagramListForMainWith :: [(String, Diagram B)]
diagramListForMainWith = map diagramEntry $ toList patternTable

