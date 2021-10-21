
import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

outputScaling = 1000 :: Double

content = outputDiagram === (outputDiagram # scale 0.5)

main :: IO ()
main = mainWith $ content # frame 0.05 # scale outputScaling

