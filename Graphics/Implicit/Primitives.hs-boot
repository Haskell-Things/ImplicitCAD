{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Implicit.Primitives where

import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3, ℝ3, ℝ2, ℝ)

class Object obj vec | obj -> vec where
    -- | The object that fills no space
    emptySpace :: obj

    -- | The object that fills the entire space
    fullSpace :: obj

    -- | Complement an Object
    complement ::
        obj     -- ^ Object to complement
        -> obj  -- ^ Result

    -- | Rounded union
    unionR ::
        ℝ        -- ^ The radius (in mm) of rounding
        -> [obj] -- ^ objects to union
        -> obj   -- ^ Resulting object

    -- | Rounded difference
    differenceR ::
        ℝ        -- ^ The radius (in mm) of rounding
        -> obj   -- ^ Base object
        -> [obj] -- ^ Objects to subtract from the base
        -> obj   -- ^ Resulting object

    -- | Rounded minimum
    intersectR ::
        ℝ        -- ^ The radius (in mm) of rounding
        -> [obj] -- ^ Objects to intersect
        -> obj   -- ^ Resulting object

    -- | Translate an object by a vector of appropriate dimension.
    translate ::
        vec      -- ^ Vector to translate by
        -> obj   -- ^ Object to translate
        -> obj   -- ^ Resulting object

    -- | Mirror an object across the hyperplane whose normal is a given vector.
    mirror ::
        vec      -- ^ Vector defining the hyperplane
        -> obj   -- ^ Object to mirror
        -> obj   -- ^ Resulting object

    -- | Scale an object
    scale ::
        vec     -- ^ Amount to scale by
        -> obj  -- ^ Object to scale
        -> obj  -- ^ Resulting scaled object

    -- | Outset of an object.
    outset ::
        ℝ        -- ^ distance to outset
        -> obj   -- ^ object to outset
        -> obj   -- ^ resulting object

    -- | Make a shell of an object.
    shell ::
        ℝ        -- ^ width of shell
        -> obj   -- ^ object to take shell of
        -> obj   -- ^ resulting shell

    -- | Get the bounding box an object
    getBox ::
        obj           -- ^ Object to get box of
        -> (vec, vec) -- ^ Bounding box

    -- | Get the implicit function for an object
    getImplicit ::
        obj           -- ^ Object to get implicit function of
        -> (vec -> ℝ) -- ^ Implicit function

    implicit ::
        (vec -> ℝ)     -- ^ Implicit function
        -> (vec, vec)  -- ^ Bounding box
        -> obj         -- ^ Resulting object

instance Object SymbolicObj2 ℝ2
instance Object SymbolicObj3 ℝ3

