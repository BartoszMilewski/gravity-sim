module Test.Main where

import Simulation
import World

main = do 
    let newWorld = advanceWorld 10 world4
    print newWorld
