module Object where

import qualified Csg
import qualified Csg.STL
import qualified Data.Text.IO as T

rBev = 10
rDial = 60

wPan = rDial + 20
dPan = wPan
dTop = 10
panAngle = pi/6
eps = 0.001
hBase = 20

cylinder = Csg.unitCylinder 8
sphere = Csg.unitSphere 8 4

hTop = hBase + (dPan * sin panAngle)

object :: Csg.BspTree
object = Csg.unionConcat $ concat [
    let c i = Csg.translate (i*dTop, 0, 0) $ Csg.rotate (1, 0, 0) (pi/2) $ 
               Csg.scale (rBev, rBev, wPan) $ cylinder
     in [(c 0.5),(c $ negate 0.5)], 
    let c i = Csg.translate (0, i*wPan, 0) $ Csg.rotate (0, 1, 0) (pi/2) $ 
               Csg.scale (rBev, rBev, dTop) $ cylinder
     in [ (c 0.5), (c $ negate 0.5)],
    pure $ Csg.scale (dTop+2*rBev, wPan, hTop) $ Csg.translate (0, 0, -0.5) Csg.unitCube,
    pure $ Csg.scale (dTop, wPan+2*rBev, hTop) $ Csg.translate (0, 0, -0.5) Csg.unitCube,
    let cu w d = (Csg.translate (0.5*(dTop + dPan  * cos panAngle), 0, 0) $ Csg.scale (d, w, 2*hTop) Csg.unitCube) `Csg.subtract` 
                    (Csg.translate (0.5 * dTop, 0, 0) $ Csg.rotate (0, 1, 0) panAngle $ Csg.uniformScale 1000 $ Csg.translate (0, 0, 0.5) Csg.unitCube) 
     in [cu wPan (dPan * cos panAngle + 2*rBev), cu (wPan + 2*rBev) (dPan * cos panAngle)],
    pure $ Csg.scale (dTop, wPan,rBev*2) Csg.unitCube,
    Csg.translate (0.5*dTop, 0, 0) . Csg.rotate (0, 1, 0) panAngle <$> (concat [
        pure $ Csg.translate (dPan/2, 0, 0) $ Csg.scale (dPan, wPan, 2*rBev) $ Csg.unitCube,
        pure $ Csg.translate (dPan, 0, 0) $ Csg.rotate (1, 0, 0) (pi/2) $ 
                Csg.scale (rBev, rBev, wPan) $ cylinder,
        let c i = Csg.translate (dPan/2, i*wPan, 0) $ Csg.rotate (0, 1, 0) (pi/2) $ 
               Csg.scale (rBev, rBev, dPan) $ cylinder 
        in [(c 0.5),(c $ negate 0.5)], 
        let s i j = Csg.translate (dPan*j, i*wPan, 0)  $ 
               Csg.scale (rBev, rBev, rBev) $ sphere
        in [s i j | i <- [0.5, -0.5], j <- [0, 1]]
      ]),
    let c i = Csg.translate (-dTop/2, i*wPan, 0) $  
               Csg.scale (rBev, rBev, hTop) $ Csg.translate (0, 0, eps-0.5) $ 
                cylinder
     in [(c 0.5),(c $ negate 0.5)], 
    let c i = Csg.translate (dTop/2 + (dPan * cos panAngle)-eps, i*wPan+eps, hBase - hTop + eps) $  
               Csg.scale (rBev, rBev, hBase) $ Csg.translate (0, 0, -0.5) $ 
                cylinder
     in [(c 0.5), (c $ negate 0.5)],
     let s i = Csg.translate (-dTop/2, i*wPan, 0)  $ 
                  Csg.scale (rBev, rBev, rBev) $ sphere
        in [s i | i <- [0.5, -0.5]]
  ]


path = "rotary-encoder.stl"

main :: IO ()
main = T.writeFile path $ Csg.STL.toSTL object
