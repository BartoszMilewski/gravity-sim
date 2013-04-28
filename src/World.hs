{-# LANGUAGE ScopedTypeVariables #-}

module World (

  -- constants
  epsilon, width, height, bigG,
  
  -- types
  Mass(..), Position(..), Velocity(..), Accel(..), Energy, Particle(..), World(..),
  
  -- read a world from a file
  readWorld,

  solarWorld,
  -- a 4-body world
  world4
  
) where
  
import Prelude hiding     (catch)

import Control.Exception  (catch)
import System.Exit        (exitFailure)

  
-- Types & constants
-- -----------------

-- For floating point comparisons
--
epsilon :: Float
epsilon = 0.001

-- Constants
--
width, height :: Int    -- extent of the window; origin is in the center
width  = 600
height = 600

-- Gravitational constant
--
bigG :: Float
bigG = 6.67428e-11                  -- in m^3 kg^(-1) s^(-2)

-- Basic physical measures
--
newtype Mass   = Mass Float  
  deriving (Show, Read) -- in kilogram
data Position  = Pos { x  :: Float, y  :: Float } deriving (Show, Read) -- in meter
data Velocity  = Vel { vx :: Float, vy :: Float } deriving (Show, Read) -- in meter/second
data Accel     = Acc { ax :: Float, ay :: Float } deriving (Show, Read) -- in meter/second^2
type Energy    = Double             -- in joule

-- We represent particles as mass points at a particular position that have a particular velocity
--
data Particle = Particle {
    mass :: Mass
  , pos  :: Position
  , vel  :: Velocity
} deriving (Show, Read)
  
-- The world state consists of three scaling factors and a set of particles.  
--
-- * The first scaling factor determines which fraction of a pixel represents one meter.
-- * The second scaling factor determines which fraction of a pixel represents one kilogram when 
--   determining the radius of the circle representing a particle.
-- * The third scaling factor determines how many simulated seconds correspond to one second of real
--   time.
--
data World = World {
    seqNum  :: Int   -- sequence number to serialize communications
  , pixInM  :: Float -- fraction of a pixel corresponding to world meter
  , pixInKg :: Float -- fraction of a pixel corresponding to world kg
  , usrToWrldTime :: Float -- user time in s to world time
  , parts   :: [Particle]
} deriving (Show, Read)


-- Setting up the world
-- --------------------

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



--
solarWorld :: World
solarWorld = World 0 distanceScale (earthMass / 10000) 750
                      [ Particle (Mass sunMass) (Pos 0 0) (Vel 0 0)
                      , Particle (Mass cometMass) (Pos cometDist 0) (Vel 0 cometVelocity)
                      , Particle (Mass cometMass) (Pos (-cometDist) (-cometDist)) (Vel (5000) (-5000))
                      , Particle (Mass cometMass) (Pos (2.0e11) (1.0e11)) (Vel (-2500) (5000))
                      , Particle (Mass earthMass) (Pos earthDist  0) (Vel 0 earthVelocity)
                      , Particle (Mass venusMass) (Pos venusDist  0) (Vel 0 venusVelocity)
                      , Particle (Mass mercuryMass) (Pos mercuryDist  0) (Vel 0 mercuryVelocity)]
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
