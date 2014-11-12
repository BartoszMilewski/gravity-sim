{-# LANGUAGE TemplateHaskell #-}
module Json where

import World
import Data.Aeson.TH
import JsonOpt

deriveJSON options0610 ''Position
deriveJSON options0610 ''Velocity
deriveJSON options0610 ''Mass
deriveJSON options0610 ''Particle
deriveJSON options0610 ''World
