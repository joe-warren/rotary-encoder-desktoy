module Object where

import qualified Csg
import qualified Csg.STL
import qualified Data.Text.IO as T

rBevOuter = 10
rBevInner = rBevOuter - wallT
rDial = 60/2
dialClearance = 1 
wallT = 4
dialInset = 5

wPan = rDial*2 + 20
dPan = wPan
dTop = 10
panAngle = pi/6
eps = 0.001
hBase = 15

dTotal = dTop + dPan * cos panAngle 

cylinder = Csg.unitCylinder 8
sphere = Csg.unitSphere 8 4

hTop = hBase + (dPan * sin panAngle)

ledsCyl = Csg.scale (5/2, 5/2, 100) $ Csg.unitCylinder 12
nLeds = 12
shaftR= 6/2

basicShape :: Double -> Csg.BspTree
basicShape rBev  = Csg.unionConcat $ concat [
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

ledHoles = Csg.unionConcat [
              Csg.translate (0, ((fromInteger i - 0.5) * wPan / (fromInteger nLeds)) -wPan/2, 0) $ ledsCyl | i <- [1..nLeds]
           ]


-- TODO, confirm usb dims
usbHole = Csg.translate (-dTop-rBevOuter, 0 , -hTop/2) $ Csg.scale (20,14, 8) $ Csg.unitCube

rotateDialIntoPlace :: Csg.BspTree -> Csg.BspTree
rotateDialIntoPlace = Csg.translate (0.5*dTop, 0, 0) . Csg.rotate (0, 1, 0) panAngle . Csg.translate (0.5*dPan, 0, rBevOuter)

dialPositive = let r = rDial+ dialClearance + wallT in 
                rotateDialIntoPlace $ Csg.scale (r, r, wallT + dialInset) $  Csg.translate (0, 0, -0.5) $ Csg.unitCylinder 16

dialNegative = let r = rDial+ dialClearance in 
                rotateDialIntoPlace $ ((Csg.scale (r, r, 2* dialInset) $ Csg.unitCylinder 32) `Csg.union` (Csg.scale (shaftR, shaftR, 40) $ Csg.unitCylinder 12))



holePositions = [ (x,y,0) | x <- [10, dTotal -10 - dTop/2], y <- ( * (wPan + rBevInner-3)) <$> [-0.5, 0.5]]

baseAttachment = let rHole = 3/2
                     rOuter = 5 
                     baseHeight = 5
                     cyl = Csg.scale (rOuter, rOuter, (hTop-baseHeight)*2) $ Csg.unitCylinder 16
                     col = cyl `Csg.subtract` (Csg.scale (rHole, rHole, 1000) $ Csg.unitCylinder 8)
                  in Csg.unionConcat $ (`Csg.translate`  col) <$> holePositions

object :: Csg.BspTree
object = ((basicShape rBevOuter `Csg.subtract` (Csg.translate (0, 0, -eps) $ (basicShape rBevInner `Csg.subtract` baseAttachment))) `Csg.union` dialPositive) `Csg.subtract` ( ledHoles `Csg.union` usbHole `Csg.union` dialNegative)



base = let h = 2 
           rScrew = 3/2
           screw = Csg.scale (rScrew, rScrew, 1000) $ Csg.unitCylinder 8
        in (Csg.translate ((dTotal-dTop)/2, 0, -hTop -20) $ (Csg.unionConcat ([
         Csg.scale (dTotal,wPan + rBevInner *2, h) $ Csg.unitCube,
         Csg.scale (dTotal+rBevInner *2,wPan, h) $ Csg.unitCube
       ] <>
         [Csg.translate (dTotal *i, wPan*j, 0) $ Csg.scale (rBevInner, rBevInner, h) $ cylinder | i <- [-0.5, 0.5], j <- [-0.5, 0.5]]
        ))) `Csg.subtract` (Csg.unionConcat $ (`Csg.translate` screw) <$> holePositions)

path = "rotary-encoder.stl"
pathBase = "rotary-encoder-base.stl"


main :: IO ()
main = do 
  --T.writeFile path $ Csg.STL.toSTL object
  T.writeFile pathBase $ Csg.STL.toSTL base
