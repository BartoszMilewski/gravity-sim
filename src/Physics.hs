module Physics (force) where

import Types

-- For floating point comparisons
epsilon :: Float
epsilon = 0.001

-- Gravitational constant
bigG :: Float
bigG = 6.67428e-11        -- in m^3 kg^(-1) s^(-2)

-- Given two particles, determine the acceleration exerted by the second on the first.
--
-- As a special case, the force is zero if both particles are closer than 
-- a minimal epsilon distance.
--
force :: Particle -> Particle -> Accel
force (Particle (Mass _) (Pos x1 y1) _) (Particle (Mass m2) (Pos x2 y2) _)
  | d < epsilon = Acc 0 0
  | otherwise   = Acc (absAccel * dx / d) (absAccel * dy / d) 
  where
    dx       = x2 - x1
    dy       = y2 - y1
    dsqr     = dx * dx + dy * dy
    d        = sqrt dsqr
    absAccel = bigG * m2 / dsqr
