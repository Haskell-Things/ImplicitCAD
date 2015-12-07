{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Released under the GNU GPL, see LICENSE

module Graphics.Implicit.Definitions where

-- a few imports for great evil :(
-- we want global IO refs.
import Data.IORef (IORef, newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.VectorSpace       

-- Let's make things a bit nicer. 
-- Following math notation ℝ, ℝ², ℝ³...
type ℝ = Float
type ℝ2 = (ℝ,ℝ)
type ℝ3 = (ℝ,ℝ,ℝ)

type ℕ = Int

-- TODO: Find a better place for this
(⋅) :: InnerSpace a => a -> a -> Scalar a
(⋅) = (<.>)

-- TODO: Find a better way to do this?
class ComponentWiseMultable a where
    (⋯*) :: a -> a -> a
    (⋯/) :: a -> a -> a
instance ComponentWiseMultable ℝ2 where
    (x,y) ⋯* (x',y') = (x*x', y*y')
    (x,y) ⋯/ (x',y') = (x/x', y/y')
instance ComponentWiseMultable ℝ3 where
    (x,y,z) ⋯* (x',y',z') = (x*x', y*y', z*z')
    (x,y,z) ⋯/ (x',y',z') = (x/x', y/y', z/z')

-- nxn matrices
-- eg. M2 ℝ = M₂(ℝ)
type M2 a = ((a,a),(a,a))
type M3 a = ((a,a,a),(a,a,a),(a,a,a))


-- | A chain of line segments, as in SVG
-- eg. [(0,0), (0.5,1), (1,0)] ---> /\
type Polyline = [ℝ2]

-- | A triangle (a,b,c) = a trinagle with vertices a, b and c
type Triangle = (ℝ3, ℝ3, ℝ3)

-- | A triangle ((v1,n1),(v2,n2),(v3,n3)) has vertices v1, v2, v3
--   with corresponding normals n1, n2, and n3
type NormedTriangle = ((ℝ3, ℝ3), (ℝ3, ℝ3), (ℝ3, ℝ3))


-- | A triangle mesh is a bunch of triangles :)
type TriangleMesh = [Triangle]

-- | A normed triangle mesh is a bunch of normed trianlges!!
type NormedTriangleMesh = [NormedTriangle]

-- $ In Implicit CAD, we consider objects as functions
-- of `outwardness'. The boundary is 0, negative is the
-- interior and positive the exterior. The magnitude is
-- how far out or in.
-- For more details, refer to http://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/

-- | A 2D object
type Obj2 = (ℝ2 -> ℝ)

-- | A 3D object
type Obj3 = (ℝ3 -> ℝ)

-- | A 2D box
type Box2 = (ℝ2, ℝ2)

-- | A 3D box
type Box3 = (ℝ3, ℝ3)

-- | Boxed 2D object
type Boxed2 a = (a, Box2)

-- | Boxed 3D object
type Boxed3 a = (a, Box3)

type BoxedObj2 = Boxed2 Obj2
type BoxedObj3 = Boxed3 Obj3

-- | A symbolic 2D object format.
--   We want to have a symbolic object so that we can 
--   accelerate rendering & give ideal meshes for simple
--   cases.
data SymbolicObj2 =
    -- Primitives
    RectR ℝ ℝ2 ℝ2
    | Circle ℝ
    | PolygonR ℝ [ℝ2]
    -- (Rounded) CSG
    | Complement2 SymbolicObj2
    | UnionR2 ℝ [SymbolicObj2]
    | DifferenceR2 ℝ [SymbolicObj2]
    | IntersectR2 ℝ [SymbolicObj2]
    -- Simple transforms
    | Translate2 ℝ2 SymbolicObj2
    | Scale2 ℝ2 SymbolicObj2
    | Rotate2 ℝ SymbolicObj2
    -- Boundary mods
    | Outset2 ℝ SymbolicObj2
    | Shell2 ℝ SymbolicObj2
    -- Misc
    | EmbedBoxedObj2 BoxedObj2
    deriving Show

-- | A symbolic 3D format!

data SymbolicObj3 = 
    -- Primitives
      Rect3R ℝ ℝ3 ℝ3
    | Sphere ℝ
    | Cylinder ℝ ℝ ℝ -- h r1 r2
    -- (Rounded) CSG
    | Complement3 SymbolicObj3
    | UnionR3 ℝ [SymbolicObj3]
    | IntersectR3 ℝ [SymbolicObj3]
    | DifferenceR3 ℝ [SymbolicObj3]
    -- Simple transforms
    | Translate3 ℝ3 SymbolicObj3
    | Scale3 ℝ3 SymbolicObj3
    | Rotate3 (ℝ,ℝ,ℝ) SymbolicObj3
    | Rotate3V ℝ ℝ3 SymbolicObj3
    -- Boundary mods
    | Outset3 ℝ SymbolicObj3
    | Shell3 ℝ SymbolicObj3
    -- Misc
    | EmbedBoxedObj3 BoxedObj3
    -- 2D based
    | ExtrudeR ℝ SymbolicObj2 ℝ
    | ExtrudeRotateR ℝ ℝ SymbolicObj2 ℝ
    | ExtrudeRM 
        ℝ                 -- rounding radius
        (Maybe (ℝ -> ℝ))  -- twist
        (Maybe (ℝ -> ℝ))  -- scale
        (Maybe (ℝ -> ℝ2)) -- translate
        SymbolicObj2      -- object to extrude
        (Either ℝ (ℝ2 -> ℝ)) -- height to extrude to
    | RotateExtrude
        ℝ                   -- Angle to sweep to
        (Maybe ℝ)           -- Loop or path (rounded corner)
        (Either ℝ2 (ℝ -> ℝ2)) -- translate function
        (Either ℝ  (ℝ -> ℝ )) -- rotate function
        SymbolicObj2      -- object to extrude
    | ExtrudeOnEdgeOf SymbolicObj2 SymbolicObj2
    deriving Show

-- | Rectilinear 2D set
type Rectilinear2 = [Box2]

-- | Rectilinear 2D set
type Rectilinear3 = [Box3]

-- | Now for something that makes me a bad person...
--   I promise I'll use it for good, not evil!
--   I don't want to reparse the program arguments 
--   everytime I want to know if XML errors are needed.

{-# NOINLINE xmlErrorOn #-}

xmlErrorOn :: IORef Bool
xmlErrorOn = unsafePerformIO $ newIORef False

errorMessage :: Int -> String -> IO()
errorMessage line msg = do
        useXML <- readIORef xmlErrorOn
        let
            msg' = "At line <line>" ++ show line ++ "</line>:" ++ msg
            -- dropXML inTag (x:xs)
            dropXML inQuote False ('"':xs) = '"':dropXML (not inQuote) False  xs
            dropXML True    _     ( x :xs) = x:dropXML True    False  xs
            dropXML False   False ('<':xs) =   dropXML False   True  xs
            dropXML False   True  ('>':xs) =   dropXML False   False xs
            dropXML inQuote True  ( _ :xs) =   dropXML inQuote True  xs
            dropXML inQuote False ( x :xs) = x:dropXML inQuote False xs
            dropXML _       _        []    = []
        if useXML 
            then putStrLn $ "<error>" ++ msg' ++ "</error>"
            else putStrLn $ dropXML False False $ msg'
        return ()

-- FIXME: document WHY this is wrong.
-- HACK: This needs to be fixed correctly someday
instance Show (a -> b) where
        show _ = "<function>"
