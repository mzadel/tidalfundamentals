
import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = mainWith $ outputDiagram === (outputDiagram # scale 0.5)

