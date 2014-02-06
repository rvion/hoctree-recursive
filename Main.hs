module Main (main) where

{-------------------------------------------------
                     Imports
-------------------------------------------------}
import Control.Monad (liftM, unless, when)

import qualified Graphics.Rendering.GLU.Raw as GLU
import qualified Graphics.Rendering.OpenGL  as GL
import qualified Graphics.UI.GLFW           as GLFW

import qualified Display                    as Display
import qualified Octree                     as O
import qualified Camera                     as C
import qualified Matrix                     as M


data Gamestate = CGamestate 
  { world        :: O.Octree
  --, worldmesh    :: O.MeshOctree
  , playerLevel  :: O.Level 
  --, currentLevel :: O.Level
  , player       :: C.Camera
  , inputs       :: Input
  }

data Input =  CInput
  { movx :: Float
  , movy :: Float
  , movz :: Float
  , mx   :: Int
  , mx'  :: Int
  , my   :: Int
  , my'  :: Int
  }


initialGamestate = CGamestate O.testOctree 5 initialCamera initialInput
initialInput     = CInput     0 0 0 0 0 0 0
initialCamera    = C.Camera eye tar up
    where eye = (0, 0, -1)  --GL.Vertex3 0 (-2) (-5) :: GL.Vertex3 GL.GLdouble
          tar = (0,  0,  0)  --GL.Vertex3 0 0 0       :: GL.Vertex3 GL.GLdouble
          up  = (0,  1,  0)  --GL.Vector3 0 1 0       :: GL.Vector3 GL.GLdouble
 
{-------------------------------------------------
                     Main
-------------------------------------------------}

main :: IO ()
main = do
    configureDisplay
    start
    stop

start :: IO ()
start =
  loop 0 0 7 O.testOctree (-1,-1) initialGamestate
  where
    loop xa ya l world (mx',my') gamestate@(CGamestate octree' level' camera' input') = do
        GLFW.resetTime
        q0         <- GLFW.keyIsPressed GLFW.KeyEsc
        q1         <- GLFW.keyIsPressed (GLFW.CharKey 'Q')
        zoomp      <- GLFW.keyIsPressed (GLFW.CharKey 'A')
        zoomm      <- GLFW.keyIsPressed (GLFW.CharKey 'Z')
        (jlr, jud) <- getJoystickDirections
        (klr, kud) <- getCursorKeyDirections
        (mx, my)   <- GLFW.getMousePosition
        
        let xa' = -kud --(xa +        jud * maxAngle) - kud
        let ya' = klr --(ya + negate jlr * maxAngle) - klr
        let l'  = l--min (max 0 (l + (if zoomp then 1 else 0) - (if zoomm then 1 else 0))) 9
        
        --(C.strafe camera' xa')
        --C.move   camera' ya'
        --C.rotateView camera' (boolToFloat zoomp) ((C.cpos camera')-(C.viewPos camera'))
        --C.rotateView camera' (boolToFloat zoomm) (C.perpendicular camera')
        
        let camera1   = C.strafe     camera' ya'
        let camera2   = C.move       camera1 xa'
        let camera3   = C.rotateView camera2 (fromIntegral (my' - my)/30) (C.perpendicular camera2)
        let newcamera = C.rotateView camera3 (fromIntegral (mx' - mx)/30) (C.upVec camera3)
         
        
        case world of 
           O.COctree m _ _ _ _ _ _ _ _ _ -> Display.draw xa ya l m (max 0 (mx'-mx), max 0 (my'-my)) newcamera--camera'
           O.COBloc  m _                 -> Display.draw xa ya l m (max 0 (mx'-mx), max 0 (my'-my)) newcamera--camera'
        
        unless (q0 || q1) $ do     
            t <- liftM (numSecondsBetweenFrames -) GLFW.getTime
            when (t > 0) (GLFW.sleep t)

            loop xa' ya' l' world (mx, my) (CGamestate octree' level' newcamera input')
      where
        maxAngle :: GL.GLdouble
        maxAngle = 1
        
        boolToFloat :: Bool -> GL.GLdouble
        boolToFloat b = if b then 1 else 0

        numSecondsBetweenFrames :: Double
        numSecondsBetweenFrames = recip (fromIntegral framesPerSecond)

        framesPerSecond :: Int
        framesPerSecond = 30

stop :: IO ()
stop = do
    GLFW.closeWindow
    GLFW.terminate
 
{-------------------------------------------------
                     Input update
-------------------------------------------------}

getJoystickDirections :: IO (GL.GLdouble, GL.GLdouble)
getJoystickDirections = return (0, 0){-do
    r <- take 2 `fmap` GLFW.getJoystickPosition GLFW.Joystick0 2
    return $
      case r of
        [x, y] -> (x, y)
        _      -> (0, 0)
-}
getCursorKeyDirections :: IO (GL.GLdouble, GL.GLdouble)
getCursorKeyDirections = do
    u <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyUp
    d <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyDown
    l <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyLeft
    r <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyRight
    return (-l + r, -u + d)
  where
    toFloat b = if b then 1 else 0

{-------------------------------------------------
                     Opengl Initialisation
-------------------------------------------------}

configureDisplay :: IO ()
configureDisplay = do
    _ <- GLFW.initialize

    _ <- GLFW.openWindow GLFW.defaultDisplayOptions
        { GLFW.displayOptions_width        = 800
        , GLFW.displayOptions_height       = 600
        , GLFW.displayOptions_numRedBits   = 8
        , GLFW.displayOptions_numGreenBits = 8
        , GLFW.displayOptions_numBlueBits  = 8
        , GLFW.displayOptions_numDepthBits = 1
        }

    GLFW.setWindowSizeCallback windowSizeCallback

    GL.clearColor    GL.$= GL.Color4 0.05 0.05 0.05 1
    GL.depthFunc     GL.$= Just GL.Less
    GL.colorMaterial GL.$= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)
    GL.shadeModel    GL.$= GL.Smooth 

    GL.lighting              GL.$= GL.Enabled
    GL.lightModelAmbient     GL.$= GL.Color4 0.2 0.2 0.2 1
    GL.position (GL.Light 0) GL.$= GL.Vertex4 (-10) 10 (-10) 0
    GL.ambient  (GL.Light 0) GL.$= GL.Color4 0.4 0.4 0.4 1
    GL.diffuse  (GL.Light 0) GL.$= GL.Color4 0.8 0.8 0.8 1
    GL.light    (GL.Light 0) GL.$= GL.Enabled

windowSizeCallback :: Int -> Int -> IO ()
windowSizeCallback w h = do
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GLU.gluPerspective 45 (fromIntegral w / fromIntegral h) 0.1 100