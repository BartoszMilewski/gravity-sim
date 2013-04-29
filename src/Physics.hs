module Physics (force, kineticEnergy, potentialEnergy, worldEnergy) where

import World

-- Given two particles, determine the acceleration exerted by the second on the first.
--
-- As a special case, the force is zero if both particles are closer than 
-- a minimal epsilon distance.
--
force :: Particle -> Particle -> Accel
force (Particle (Mass m1) (Pos x1 y1) _) (Particle (Mass m2) (Pos x2 y2) _)
  | d < epsilon = Acc 0 0
  | otherwise   = Acc (absAccel * dx / d) (absAccel * dy / d) 
  where
    dx       = x2 - x1
    dy       = y2 - y1
    dsqr     = (dx * dx) + (dy * dy)
    d        = sqrt dsqr
    absAccel = bigG * m2 / dsqr

-- Compute the energy of a particle
--
kineticEnergy :: Particle -> Energy
kineticEnergy (Particle (Mass m) _ (Vel vx vy)) = realToFrac $ 0.5 * m * vsqr
  where
    vsqr = vx * vx + vy * vy

-- The potential energy of a system of two masses
--
-- As a special case, the energy is zero if both particles are closer than 
-- a minimal epsilon distance
--
potentialEnergy :: Particle -> Particle -> Energy
potentialEnergy (Particle (Mass m1) pos1@(Pos x1 y1) _) 
                (Particle (Mass m2) pos2@(Pos x2 y2) _)
  | d < epsilon = 0
  | otherwise   = - (realToFrac bigG * realToFrac m1 * realToFrac m2 / realToFrac d)
  where
    dx       = x2 - x1
    dy       = y2 - y1
    dsqr     = (dx * dx) + (dy * dy)
    d        = sqrt dsqr

-- Compute the overall kinetic and gravitational potential energy of a particle world
--
worldEnergy :: World -> Energy
worldEnergy world =
  let ps = parts world in
    sumMap kineticEnergy ps + sumMap (\p -> sumMap (potentialEnergy p) ps) ps / 2
                              -- divide by 2 as we should count every pair only once
  where
    sumMap f = sum . map f
