
import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Ratio
import Data.Map (Map,fromList)

outputScaling = 1000 :: Double

numTicks = 7

colourTable :: Map String Int
colourTable = fromList [
     ("a", 2)
    ,("b", 2)]

outputDiagram :: Diagram B
outputDiagram = patternDiagram [("a", 1/7, 2/7), ("b", 3/7, 5/7)] numTicks colourTable

content = outputDiagram === (outputDiagram # scale 0.5)

main :: IO ()
main = mainWith $ content # frame 0.05 # scale outputScaling

