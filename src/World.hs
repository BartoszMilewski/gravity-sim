{-# LANGUAGE ScopedTypeVariables #-}

module World (
  -- read a world from a file
  readWorld,

  solarWorld,
  -- a 4-body world
  world4 
) where

import Control.Exception  (catch)
import System.Exit        (exitFailure)
import Types

width, height :: Int    -- extent of the window; origin is in the center
width  = 600
height = 600

-- Read a world model from the given file
--
readWorld :: FilePath -> IO World
readWorld fname
  = do
      contents <- readFile fname
      readIO contents
   `catch` \(exc::IOError) ->
     do 
       putStrLn $ "Fatal error: can't read world description\n" ++ show exc
       exitFailure

solarWorld :: World
solarWorld = World 0 distanceScale (earthMass / 10000) 750
                      [ Particle (Mass sunMass) 
                                 (Pos 0 0) (Vel 0 0)
                      , Particle (Mass cometMass) 
                                 (Pos cometDist 0) (Vel 0 cometVelocity)
                      , Particle (Mass cometMass) 
                                 (Pos (-cometDist) (-cometDist)) (Vel 5000 (-5000))
                      , Particle (Mass cometMass) 
                                 (Pos 2.0e11 1.0e11) (Vel (-2500) 5000)
                      , Particle (Mass earthMass) 
                                 (Pos earthDist  0) (Vel 0 earthVelocity)
                      , Particle (Mass venusMass) 
                                 (Pos venusDist  0) (Vel 0 venusVelocity)
                      , Particle (Mass mercuryMass) 
                                 (Pos mercuryDist  0) (Vel 0 mercuryVelocity)]
  where
    sunMass         = 1.9891e30
    earthDist       = 152098232e3   -- Aphelion
    earthMass       = 5.9736e24
    earthVelocity   = 29.78e3
    venusDist       = 1.08e11
    venusMass       = 4.869e24
    venusVelocity   = 35e3
    mercuryDist     = 4.6e10
    mercuryMass     = 3.3e23
    mercuryVelocity = 49.88e3
    cometDist       = 2.0e11
    cometMass       = 1.0e20
    cometVelocity   = 7000
    --
    distanceScale = (fromIntegral height * 0.4) / earthDist 

world4 :: World
world4 = World 0 0.5 9.42590890872e11 1
               [ Particle (Mass 1e16) (Pos (-100) 30) (Vel 0 (-65))
               , Particle (Mass 1e16) (Pos 240 0)     (Vel (-40) 30)
               , Particle (Mass 1e16) (Pos 50 200)    (Vel 0 (-30))
               , Particle (Mass 1e15) (Pos 0 (-300))  (Vel 0 5)]
