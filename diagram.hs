
import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Map (Map,fromList)
import qualified Sound.Tidal.Context as T

outputScaling = 1000 :: Double

colourTable1 :: Map String Int
colourTable1 = fromList [
     ("a", 2)
    ,("b", 2)]

colourTable2 :: Map String Int
colourTable2 = fromList [
     ("a", 0)
    ,("b", 2)]

pat1 = T.s $ T.parseBP_E "a a b"
pat2 = T.s $ T.parseBP_E "a b b"

outputDiagram1 :: Diagram B
outputDiagram1 = patternDiagram pat1 7 colourTable1

outputDiagram2 :: Diagram B
outputDiagram2 = patternDiagram pat2 4 colourTable2

content = outputDiagram1 === (outputDiagram2 # scale 0.5)

main :: IO ()
main = mainWith $ content # frame 0.05 # scale outputScaling

