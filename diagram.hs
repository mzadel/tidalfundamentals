
import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Ratio
import Data.Map (Map,fromList)

outputScaling = 1000 :: Double

colourTable1 :: Map String Int
colourTable1 = fromList [
     ("a", 2)
    ,("b", 2)]

colourTable2 :: Map String Int
colourTable2 = fromList [
     ("a", 3)
    ,("b", 5)]

outputDiagram1 :: Diagram B
outputDiagram1 = patternDiagram [("a", 1/7, 2/7), ("b", 3/7, 5/7)] 7 colourTable1

outputDiagram2 :: Diagram B
outputDiagram2 = patternDiagram [("a", 0, 3/8), ("b", 2/4, 3/4)] 4 colourTable2

content = outputDiagram1 === (outputDiagram2 # scale 0.5)

main :: IO ()
main = mainWith $ content # frame 0.05 # scale outputScaling

