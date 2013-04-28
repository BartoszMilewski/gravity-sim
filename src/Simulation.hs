module Simulation (moveParticle, accelerate, advanceWorld) where
  
import World
import Physics
import Control.Parallel.Strategies

-- Move a particle according to its velocity for the given number of (simulated) seconds.
--
moveParticle :: Float -> Particle -> Particle
moveParticle dt (Particle m (Pos x y) (Vel vx vy)) =
  Particle m (Pos (x + dt * vx) (y + dt * vy)) (Vel vx vy)
    
-- Accelerate a particle in dependence on the gravitational force exerted by all other particles for
-- the given number of (simulated) seconds.
-- force :: Particle -> Particle -> Accel
accelerate :: Float -> [Particle] -> [Particle]
accelerate dt particles =
    parMap rseq acc particles
  where
    acc particle@(Particle m (Pos x y) (Vel vx vy)) =
      foldl addAcc particle particles
    addAcc myParticle@(Particle m pos (Vel vx vy)) otherParticle =
      let (Acc ax ay) = force myParticle otherParticle
      in
        Particle m pos (Vel (vx + dt * ax) (vy + dt * ay))
  

-- Progressing the world state
--
advanceWorld :: Float -> World -> World
advanceWorld dtReal world =
  let dt = dtReal * usrToWrldTime world
      newParticles = map (moveParticle dt) (accelerate dt $ parts world)
  in
      world { parts = newParticles }
