module Display
  ( draw
  ) where

import qualified Cube                      as Cube
import qualified Octree                    as Octree
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import qualified Camera                    as C

draw :: GL.GLdouble -> GL.GLdouble -> Double -> IO () -> (Int, Int) -> C.Camera -> IO ()
draw xa ya l mesh (mx, my) camera' = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    --GL.loadIdentity
    GL.preservingMatrix $ do
        GL.scale w w w
        --GL.lookAt eye tar up
        --GL.translate (GL.Vector3 (Octree.toFloat (xa/10)) 0 0)
        --GL.translate (GL.Vector3 0 (Octree.toFloat (ya/10)) 0)
        --GL.translate (GL.Vector3 (Octree.toFloat ya) 0 0)

        --GL.rotate (realToFrac (3*xa)) xVector3
        --GL.rotate (realToFrac (3*ya)) yVector3
        C.cameraLook camera'
        mesh
    GLFW.swapBuffers
  where
    --eye = GL.Vertex3 0 (-2) (-5) :: GL.Vertex3 GL.GLdouble
    --tar = GL.Vertex3 0 0 0       :: GL.Vertex3 GL.GLdouble
    --up  = GL.Vector3 0 1 0       :: GL.Vector3 GL.GLdouble
    xVector3 = GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat
    yVector3 = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat
    zVector3 = GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat
    w = 1 / (2 ** (Octree.toFloat l))
