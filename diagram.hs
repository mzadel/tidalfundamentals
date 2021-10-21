
import TidalPatternDiagram
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Map (Map,fromList)
import qualified Sound.Tidal.Context as T
import DiagramTable

main :: IO ()
main = mainWith $ diagramListForMainWith

