module Test.Main where

import Simulation

main :: IO ()
main = do 
    print $ duplicate $ PtSet [1, 2, 3]
    --let newWorld = advanceWorld 10 world4
    --print newWorld
