{-# LANGUAGE TemplateHaskell #-}
module Json where

import World
import Data.Aeson
import Data.Aeson.TH

deriveJSON id ''Position
deriveJSON id ''Velocity
deriveJSON id ''Mass
deriveJSON id ''Particle
deriveJSON id ''World
