
{-------------------------------------------------
                     Exports
-------------------------------------------------}

module Octree
  ( Octree (..)
  , Level
  , Quad
  , quadList   -- quadList :: Octree -> Level -> Position -> Quads
  , drawOctree
      -- Test / Debug / Examples
  , testOctree -- Octree
  , ploplist
  , ploplist'
      -- Binary Search tree
  , leaf       -- Quad -> Tree
  , size       -- Tree -> Int
  , insert     -- Quad -> Tree -> Tree
  , treeToList -- Tree -> [Quad] 
      -- Util
  , toFloat    -- :: (Real a, Floating a) => a -> GL.GLfloat
  ) where

{-------------------------------------------------
                     Imports
-------------------------------------------------}

import Control.Monad
import qualified Data.Map                  as M
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.List                 as List
import qualified Data.Sequence             as S
import qualified Data.Foldable             as F -- toList des Seq

{-------------------------------------------------
                     Data / types
-------------------------------------------------}

data Tree = Vide | Node { liste :: [Quad], val :: Int, left, right ::Tree }
data Face = Front | Back | Top | Bottom | Left | Right deriving Eq

data Octree = COctree { mesh :: IO (), bloc :: Bloc, o000, o001, o010, o011, o100, o101, o110, o111 :: Octree}
            | COBloc  { mesh :: IO (), bloc :: Bloc }

data Bloc = CBloc { material :: Double, faces :: Faces}

               --front back  top   bottom right left
type Faces     = (Bool, Bool, Bool, Bool,  Bool, Bool)
                
type Level     = Double
type Path      = [Octant]
type Octant    = (Bool, Bool, Bool)
type Direction = (Double, Double, Double)
type Position  = (GL.GLfloat, GL.GLfloat, GL.GLfloat)
type Context   = [(Octree, Octant)]
type Quad      = (Position, Position, Position, Position, Position)
type Quads     = ([Quad],[Quad],[Quad],[Quad],[Quad],[Quad])
type QuadsSorted  = ([[Quad]],[[Quad]],[[Quad]],[[Quad]],[[Quad]],[[Quad]])
type QuadsSS   = ( S.Seq [Quad], S.Seq [Quad], S.Seq [Quad], S.Seq [Quad], S.Seq [Quad], S.Seq [Quad])
type QuadsSSS  = ( Tree, Tree, Tree, Tree, Tree, Tree)
{-------------------------------------------------
                       Octree
-------------------------------------------------}

drawOctree :: GL.GLfloat -> Quads -> IO ()
drawOctree s (a,b,c,d,e,f)= do
    --GL.scale s s s
    GL.renderPrimitive GL.Quads $ sequence_ $ concat $
        [[GL.color red]
        ,concat (map toGlQuad a)
        ,[GL.color green]
        ,concat (map toGlQuad b)
        ,[GL.color blue]
        ,concat (map toGlQuad c)
        ,[GL.color red]
        ,concat (map toGlQuad d)
        ,[GL.color green]
        ,concat (map toGlQuad e)
        ,[GL.color blue]
        ,concat (map toGlQuad f)]
           where 
           toGlQuad ((n1,n2,n3),(x1,y1,z1),(x2,y2,z2),(x3,y3,z3),(x4,y4,z4)) =
             [GL.normal (GL.Normal3 n1 n2 n3),
             GL.vertex (GL.Vertex3 x1 y1 z1),
             GL.vertex (GL.Vertex3 x2 y2 z2),
             GL.vertex (GL.Vertex3 x3 y3 z3),
             GL.vertex (GL.Vertex3 x4 y4 z4)]
           red, green, blue :: GL.Color4 GL.GLfloat
           red   = GL.Color4 1 0 0 1
           green = GL.Color4 0 1 0 1
           blue  = GL.Color4 0 0 1 1

isSolid :: Octree -> Bool
isSolid ( COctree _ bloc _ _ _ _ _ _ _ _) = isSolid' bloc
isSolid ( COBloc _ bloc )                 = isSolid' bloc

isSolid' :: Bloc -> Bool
isSolid' (CBloc material _ ) = if material > 0 then True else False

--front back  top   bottom right left
simplify :: Octree -> Path -> Level -> Octree
simplify octree path 0 = subOctree octree path
simplify octree path l
  | isBlocOctree (subOctree octree path) = COBloc  (return ()) (newB octree path)
  | otherwise                            = COctree (return ()) (newB octree path)
                                                                  (simplify octree ((False,False,False):path) (l-1))
                                                                  (simplify octree ((False,False,True ):path) (l-1))
                                                                  (simplify octree ((False,True ,False):path) (l-1))
                                                                  (simplify octree ((False,True ,True ):path) (l-1))
                                                                  (simplify octree ((True ,False,False):path) (l-1))
                                                                  (simplify octree ((True ,False,True ):path) (l-1))
                                                                  (simplify octree ((True ,True ,False):path) (l-1))
                                                                  (simplify octree ((True ,True ,True ):path) (l-1))

newB :: Octree -> Path -> Bloc
newB octree path = CBloc m (ba, bb, bc, bd, be, bf) 
  where ba =  if (isSolid (subOctree octree (neighbor path (-1, 0, 0)))) then False else True
        bb =  if (isSolid (subOctree octree (neighbor path ( 1, 0, 0)))) then False else True
        bc =  if (isSolid (subOctree octree (neighbor path ( 0,-1, 0)))) then False else True
        bd =  if (isSolid (subOctree octree (neighbor path ( 0, 1, 0)))) then False else True
        be =  if (isSolid (subOctree octree (neighbor path ( 0, 0,-1)))) then False else True
        bf =  if (isSolid (subOctree octree (neighbor path ( 0, 0, 1)))) then False else True
        (CBloc m f) = blocOctree ( subOctree octree path )

blocOctree :: Octree -> Bloc
blocOctree ( COBloc  mesh bloc)                 = bloc
blocOctree ( COctree mesh bloc a b c d e f g h) = bloc 

-- meshOctree :: Octree -> Bloc
-- meshOctree ( COBloc  mesh bloc)                 = bloc
-- meshOctree ( COctree mesh bloc a b c d e f g h) = bloc 

isBlocOctree ::Octree -> Bool
isBlocOctree ( COBloc  mesh bloc)                 = True
isBlocOctree ( COctree mesh bloc a b c d e f g h) = False 
{-------------------------------------------------
               Octree Navigation
-------------------------------------------------}

{-
    type Level     = Double
    type Path      = [Octant]
    type Octant    = (Bool, Bool, Bool)
-}
--retourne
neighbor :: Path -> Direction -> Path
neighbor [] _ = []
neighbor a dir= neighbor' a' dir --reverse (neighbor a' dir)
   where
   a' = a --reverse a
   neighbor' :: Path -> Direction -> Path
   neighbor' p@((a1,a2,a3):b) (dx, dy,dz)
     | p == [] = []
     | dx == 0 && dy==0 && dz==0 = ((a1,a2,a3):b)
     | dx <0 = if a1 then neighbor' ((False,a2,a3):b)                     (dx+1, dy, dz)
                     else neighbor' ((True,a2,a3) :(neighbor b (-1,0,0))) (dx+1, dy, dz)
     | dx >0 = if a1 then neighbor' ((False,a2,a3):(neighbor b ( 1,0,0))) (dx-1, dy, dz)
                     else neighbor' ((True,a2,a3) :b)                     (dx-1, dy, dz)
     | dy <0 = if a2 then neighbor' ((a1,False,a3):b)                     (dx, dy, dz+1)
                     else neighbor' ((a1,True,a3) :(neighbor b (0,-1,0))) (dx, dy, dz+1)
     | dy >0 = if a2 then neighbor' ((a1,False,a3):(neighbor b (0, 1,0))) (dx, dy, dz-1)
                     else neighbor' ((a1,True,a3) :b)                     (dx, dy, dz-1)
     | dz <0 = if a3 then neighbor' ((a1,a2,False):b)                     (dx, dy, dz+1)
                     else neighbor' ((a1,a2,True) :(neighbor b (0,0,-1))) (dx, dy, dz+1)
     | dz >0 = if a3 then neighbor' ((a1,a2,False):(neighbor b (0,0, 1))) (dx, dy, dz-1)
                     else neighbor' ((a1,a2,True) :b)                     (dx, dy, dz-1)
     | otherwise = []

-- retourne l' octree-fils situé au chemin indiqué
subOctree :: Octree -> Path -> Octree
subOctree octree [] = octree
--subOctree octree (a:b) = subOctree (octant a octree) b
subOctree octree@(COctree _ bloc _ _ _ _ _ _ _ _) (a:b) = subOctree (octant a octree) b
subOctree octree@(COBloc _ bloc )                 (a:b) = octree

-- retourne l'un des 8 octree fils
octant :: Octant -> Octree -> Octree
octant (False, False, False) = o000
octant (False, False, True ) = o001
octant (False, True , False) = o010
octant (False, True , True ) = o011
octant (True , False, False) = o100
octant (True , False, True ) = o101
octant (True , True , False) = o110
octant (True , True , True ) = o111
 
{-------------------------------------------------
                    Quads stuff
-------------------------------------------------}
  
blocToQuad :: Bloc -> Position -> Level -> QuadsSSS -> QuadsSSS--(Quads,GL.GLfloat)
blocToQuad bloc@(CBloc m (a, b, c, d, e, f))  (x,y,z) level (seqA, seqB, seqC, seqD, seqE, seqF)= 
  (if a then (insert (((0,0,1), (x , y , zp), (xp, y , zp), (xp, yp, zp), (x , yp, zp))) (toInt zp) seqA) else seqA --front
  ,if b then (insert (((0,0,-1),(x , y , z ), (x , yp, z ), (xp, yp, z ), (xp, y , z ))) (toInt z ) seqB) else seqB --back
  ,if c then (insert (((0,1,0), (x , yp, z ), (x , yp, zp), (xp, yp, zp), (xp, yp, z ))) (toInt yp) seqC) else seqC --top
  ,if d then (insert (((0,-1,0),(x , y , z ), (xp, y , z ), (xp, y , zp), (x , y , zp))) (toInt y ) seqD) else seqD --bottom
  ,if e then (insert (((1,0,0), (xp, y , z ), (xp, yp, z ), (xp, yp, zp), (xp, y , zp))) (toInt xp) seqE) else seqE --right
  ,if f then (insert (((-1,0,0),(x , y , z ), (x , y , zp), (x , yp, zp), (x , yp, z ))) (toInt x ) seqF) else seqF) --left
  where dl = toFloat (2 ** (level-1))
        xp = x+dl
        yp = y+dl
        zp = z+dl

-- quad à insérer -> dans cette liste -> sortie
concatClever :: [Quad]         -> [Quad]           -> [Quad]
concatClever new liste = second (concatClever' new liste [])
  where 
    second (a,b,c) = b
    concatClever' :: [Quad] -> [Quad] -> [Quad] -> ([Quad],[Quad],[Quad])
    concatClever' [] b dt = ([], b++dt, [])
    concatClever' (q1@(n,v0,v1,v2,v3):xs) [] dt = concatClever' xs (q1:dt) [] 
    concatClever' k@(q1@(n,v0,v1,v2,v3):xs) (q2@(n',v0',v1',v2',v3'):xs') dt
      | (v0 == v1' && v3 == v2') = concatClever' ((n ,v0',v1 ,v2 ,v3'):xs) (xs' ++ dt) []
      | (v3 == v0' && v2 == v1') = concatClever' ((n ,v0 ,v1 ,v2',v3'):xs) (xs' ++ dt) []
      | (v1 == v0' && v2 == v3') = concatClever' ((n ,v0 ,v1',v2',v3 ):xs) (xs' ++ dt) []
      | (v0 == v3' && v1 == v2') = concatClever' ((n ,v0',v1',v2 ,v3 ):xs) (xs' ++ dt) []
      | otherwise                = concatClever' k (xs') (q2:dt)

quadList :: Octree -> Level -> Position -> Quads
quadList octree level position =  ( treeToList a
                                  , treeToList b
                                  , treeToList c
                                  , treeToList d
                                  , treeToList e
                                  , treeToList f)
  where
  (a,b,c,d,e,f) = (quadList' octree level position emptys)
  
  emptys :: QuadsSSS
  emptys = (emptyu,emptyu,emptyu,emptyu,emptyu,emptyu)
  emptyu = Node [] 0 Vide Vide
  
  quadList' :: Octree -> Level -> Position -> QuadsSSS -> QuadsSSS
  quadList' octree@( COctree _ bloc o000 o001 o010 o011 o100 o101 o110 o111) 0 p seq = if (isSolid octree) then (blocToQuad bloc p 0 seq) else seq
  quadList' octree@( COBloc _ bloc )                                         l p seq = if (isSolid octree) then (blocToQuad bloc p l seq) else seq
  quadList' octree@( COctree _ bloc o000 o001 o010 o011 o100 o101 o110 o111) l (x,y,z) seq =  
    (quadList' o000 (l-1) (x   ,y   ,z   )
      (quadList' o001 (l-1) (x   ,y   ,z+dl) 
        (quadList' o010 (l-1) (x   ,y+dl,z   )
           (quadList' o011 (l-1) (x   ,y+dl,z+dl)
             (quadList' o100 (l-1) (x+dl,y   ,z   )
               (quadList' o101 (l-1) (x+dl,y   ,z+dl)
                 (quadList' o110 (l-1) (x+dl,y+dl,z   )
                   (quadList' o111 (l-1) (x+dl,y+dl,z+dl) seq ))))))))
    where dl = toFloat (2 ** (l-2))
        
toQuadList :: Quads -> [Quad]
toQuadList (a, b, c, d, e, f) = (a ++ b ++ c ++ d ++ e ++ f)

{-------------------------------------------------
                    Quads stuff Old
-------------------------------------------------}
  
blocToQuadOld :: Bloc -> Position -> Level -> QuadsSS -> QuadsSS--(Quads,GL.GLfloat)
blocToQuadOld bloc@(CBloc m (a, b, c, d, e, f))  (x,y,z) level (seqA, seqB, seqC, seqD, seqE, seqF)= 
  (if a then (S.adjust (\blob -> concatClever ([((0,0,1), (x , y , zp), (xp, y , zp), (xp, yp, zp), (x , yp, zp))]) blob) (toInt zp) seqA) else seqA --front
  ,if b then (S.adjust (\blob -> concatClever ([((0,0,-1),(x , y , z ), (x , yp, z ), (xp, yp, z ), (xp, y , z ))]) blob) (toInt z ) seqB) else seqB --back
  ,if c then (S.adjust (\blob -> concatClever ([((0,1,0), (x , yp, z ), (x , yp, zp), (xp, yp, zp), (xp, yp, z ))]) blob) (toInt yp) seqC) else seqC --top
  ,if d then (S.adjust (\blob -> concatClever ([((0,-1,0),(x , y , z ), (xp, y , z ), (xp, y , zp), (x , y , zp))]) blob) (toInt y ) seqD) else seqD --bottom
  ,if e then (S.adjust (\blob -> concatClever ([((1,0,0), (xp, y , z ), (xp, yp, z ), (xp, yp, zp), (xp, y , zp))]) blob) (toInt xp) seqE) else seqE --right
  ,if f then (S.adjust (\blob -> concatClever ([((-1,0,0),(x , y , z ), (x , y , zp), (x , yp, zp), (x , yp, z ))]) blob) (toInt x ) seqF) else seqF) --left
  where dl = toFloat (2 ** (level-1))
        xp = x+dl
        yp = y+dl
        zp = z+dl

quadListOld :: Octree -> Level -> Position -> Quads
quadListOld octree level position =  ( concat (F.toList a)
                                  , concat (F.toList b)
                                  , concat (F.toList c)
                                  , concat (F.toList d)
                                  , concat (F.toList e)
                                  , concat (F.toList f))
  where
  (a,b,c,d,e,f) = (quadListOld' octree level position emptys)
  
  emptys :: QuadsSS
  emptys = (emptyu,emptyu,emptyu,emptyu,emptyu,emptyu)
  emptyu = S.fromList (replicate (floor(2 ** level)) [])
  
  quadListOld' :: Octree -> Level -> Position -> QuadsSS -> QuadsSS
  quadListOld' octree@( COctree _ bloc o000 o001 o010 o011 o100 o101 o110 o111) 0 p seq = if (isSolid octree) then (blocToQuadOld bloc p 0 seq) else seq
  quadListOld' octree@( COBloc _ bloc )                                         l p seq = if (isSolid octree) then (blocToQuadOld bloc p l seq) else seq
  quadListOld' octree@( COctree _ bloc o000 o001 o010 o011 o100 o101 o110 o111) l (x,y,z) seq =  
    (quadListOld' o000 (l-1) (x   ,y   ,z   )
      (quadListOld' o001 (l-1) (x   ,y   ,z+dl) 
        (quadListOld' o010 (l-1) (x   ,y+dl,z   )
           (quadListOld' o011 (l-1) (x   ,y+dl,z+dl)
             (quadListOld' o100 (l-1) (x+dl,y   ,z   )
               (quadListOld' o101 (l-1) (x+dl,y   ,z+dl)
                 (quadListOld' o110 (l-1) (x+dl,y+dl,z   )
                   (quadListOld' o111 (l-1) (x+dl,y+dl,z+dl) seq ))))))))
    where dl = toFloat (2 ** (l-2))

{-------------------------------------------------
                  Util
-------------------------------------------------}

toFloat :: (Real a, Floating a) => a -> GL.GLfloat
toFloat = realToFrac

toInt :: GL.GLfloat -> Int
toInt = round   
{-------------------------------------------------
                  Binary Search tree
-------------------------------------------------}

{-
    data Tree = Vide | Node { liste :: [Quad], val :: GL.GLfloat, left, right ::Tree }
-}

leaf :: Quad -> Int -> Tree
leaf  p@(a,b,c,d,e) id = Node [p] id Vide Vide

size :: Tree -> Int
size Vide = 0
size (Node _ _ tl tr) = 1 + size tl + size tr

insert :: Quad -> Int -> Tree -> Tree
insert a id Vide = leaf a id
insert p@(a,b,c,d,e) id' (Node liste id xl xr) 
  | id'== id    = Node (concatClever [p] liste) id xl            xr
  | id' > id    = Node liste                    id (insert p id' xl) xr
  | otherwise   = Node liste                    id xl (insert p id' xr)

treeToList :: Tree -> [Quad]
treeToList Vide = []
treeToList (Node x _ xl xr) = x ++ treeToList xl ++ treeToList xr


{-------------------------------------------------
               Test / Debug / Examples
-------------------------------------------------}

testOctree :: Octree
testOctree =  COctree (drawOctree w (quadList testOctree {-(quadList (simplify testOctree [] 0) 5 (0,0,0))-} 7 (0,0,0) ))
                      (CBloc 0 a) 
                      testOctree
                      testOctree
                      testOctree 
                      testOctree'
                      testOctree 
                      testOctree''
                      testOctree''  
                      testOctree'
  where 
  a = (True, True, True, True, True, True)
  testOctree'  = COBloc (return ()) (CBloc 0 a)
  testOctree'' = COBloc (return ()) (CBloc 2 a)
  w = 1 / (2 ** (Octree.toFloat 7))

ploplist :: [Quad]
ploplist    = toQuadList  (quadList    (simplify testOctree [] 3) 4 (0,0,0)) 

ploplist' :: [Quad]
ploplist'   = toQuadList  (quadList testOctree 5 (0,0,0))

ploplist'' :: [Quad]
ploplist''  = toQuadList  (quadList  testOctree 5 (0,0,0))

{-testOctree :: Octree
testOctree =  COctree (drawOctree w (quadList testOctree 5 (0,0,0))) 
                      (CBloc 2 a) 
                      testOctree'
                      testOctree
                      testOctree 
                      testOctree''
                      testOctree'' 
                      testOctree''
                      testOctree  
                      testOctree-}
