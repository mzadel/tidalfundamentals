{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

outputScaling = 50 :: Double

myCircle :: Diagram B
myCircle = circle 1 # fc green

main = mainWith (myCircle # scale outputScaling)

