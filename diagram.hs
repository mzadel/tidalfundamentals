
import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Ratio

outputScaling = 1000 :: Double

numTicks = 7

outputDiagram :: Diagram B
outputDiagram = patternDiagram [("a", 1/7, 2/7), ("b", 3/7, 5/7)] numTicks

content = outputDiagram === (outputDiagram # scale 0.5)

main :: IO ()
main = mainWith $ content # frame 0.05 # scale outputScaling

