-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ObjectUtil.GetBox2 (getBox2, getBox2R) where

import Prelude(flip, fmap, Eq, (==), (||), unzip, minimum, maximum, ($), (/), (-), (+), (*), cos, sin, sqrt, min, max, (<), (<>), pi, atan2, (==), (>), show, (&&), otherwise, error)

import Graphics.Implicit.Definitions
    ( SymbolicObj2(..),
      SharedObj(IntersectR, Complement, UnionR, DifferenceR),
      Box2,
      ℝ2,
      ℝ,
      minℝ )


import Data.Fixed (mod')
import Graphics.Implicit.ObjectUtil.GetBoxShared (emptyBox, corners, outsetBox, intersectBoxes, pointsBox, getBoxShared, unionBoxes)


-- Get a Box2 around the given object.
getBox2 :: SymbolicObj2 -> Box2
-- Primitives
getBox2 (SquareR _ size) = ((0, 0), size)
getBox2 (Circle r) = ((-r, -r), (r,r))
getBox2 (PolygonR _ points) = pointsBox points
-- (Rounded) CSG
-- Simple transforms
getBox2 (Rotate2 θ symbObj) =
    let
        ((x1,y1), (x2,y2)) = getBox2 symbObj
        rotate (x,y) = (x*cos θ - y*sin θ, x*sin θ + y*cos θ)
    in
        pointsBox [ rotate (x1, y1)
                  , rotate (x1, y2)
                  , rotate (x2, y1)
                  , rotate (x2, y2)
                  ]
getBox2 (Shared2 obj) = getBoxShared obj


-- | Define a Box2 around the given object, and the space it occupies while rotating about the center point.
--   Note: No implementations for SquareR, Translate2, or Scale2 as they would be identical to the fallthrough.
getBox2R :: SymbolicObj2 -> ℝ -> Box2
getBox2R (Circle r) _ = getBox2 $ Circle r
getBox2R (PolygonR _ points) deg =
  let
    pointRBoxes = [ pointRBox point deg | point <- points ]
    (pointValsMin, pointValsMax) = unzip pointRBoxes
    (pointValsX, pointValsY) = unzip (pointValsMin <> pointValsMax)
  in
    ((minimum pointValsX, minimum pointValsY), (maximum pointValsX, maximum pointValsY))
getBox2R (Shared2 (Complement symObj)) _ = getBox2 $ Shared2 (Complement symObj)
getBox2R (Shared2 (UnionR r symObjs)) deg =
    unionBoxes r $ fmap (flip getBox2R deg) symObjs
getBox2R (Shared2 (DifferenceR _ symObj _)) deg = getBox2R symObj deg
getBox2R (Shared2 (IntersectR r symObjs)) deg =
  let
    boxes = [ getBox2R obj deg| obj <- symObjs ]
  in
    outsetBox r $ intersectBoxes boxes
-- FIXME: implement Rotate2.
-- Fallthrough: rotate the points of the containing box. no rounding.
getBox2R symObj deg =
  let
    origBox = getBox2 symObj
    points  = corners origBox
  in
    getBox2R (PolygonR 0 points) deg

data Quadrant  = UpperRight | UpperLeft | LowerRight | LowerLeft
  deriving Eq
data Axis      = PosX | PosY | NegX | NegY
  deriving Eq
data Position  = OnAxis Axis | InQuadrant Quadrant | CenterPoint
data HasRotation = Rotation Direction | None
  deriving Eq
data Direction = Clockwise | CounterClockwise
  deriving Eq

-- | put a box around a point, and all of the locations it will be at during an x degree arc around (0,0).
pointRBox :: ℝ2 -> ℝ -> Box2
pointRBox (xStart, yStart) travel =
  let
    k :: ℝ
    k = pi/180
    -- determine the distance of our input point from from the axis of rotation.
    distance = sqrt $ xStart*xStart + yStart*yStart
    -- radian starting position.
    θstart = atan2 yStart xStart
    -- logical starting position
    startPosition = positionOf distance $ absrad θstart

    -- take the input point. rotate it. see where it stops.

    -- how far we should rotate our point.
    rotationAmount = travel * k
    -- what direction are we rotating.
    rotationDirection = case travel of
      polarity | polarity > 0  -> Rotation CounterClockwise
               | polarity == 0 -> None
      _                        -> Rotation Clockwise
    -- stopping position of our point.
    θstop = absrad $ θstart + rotationAmount
    stopPosition = positionOf distance θstop
    (xStop, yStop) =
      case positionOf distance θstop of
        CenterPoint -> (0,0)
        OnAxis PosX -> (distance,0)
        OnAxis PosY -> (0,distance)
        OnAxis NegX -> (-distance,0)
        OnAxis NegY -> (0,-distance)
        InQuadrant _ -> ( distance*cos θstop, distance*sin θstop)

    -- observe what the initial position was, and what the end position is.
    -- check which quadrants they're in, and what direction the rotation was.

    (minX, minY, maxX, maxY) = (min xStart xStop, min yStart yStop, max xStart xStop, max yStart yStop)
    positionOf :: ℝ -> ℝ -> Position
    positionOf d θpos
      | d < minℝ                      = CenterPoint
      | θpos == 0 || θpos == 360*k    = OnAxis PosX
      | θpos == 90*k                  = OnAxis PosY
      | θpos == 180*k || θpos == -0   = OnAxis NegX
      | θpos == 270*k                 = OnAxis NegY
      | θpos > 0 && θpos < 90*k       = InQuadrant UpperRight
      | θpos > 90*k && θpos < 180*k   = InQuadrant UpperLeft
      | θpos > 180*k && θpos < 270*k  = InQuadrant LowerLeft
      | θpos > 270*k && θpos < 360*k  = InQuadrant LowerRight
      | otherwise                     = error $ "illegal position in positionOf: " <> show (θpos*k) <> " pos: " <> show θpos <> " d: " <> show d
    -- returns position around a circle in radians, from 0 to 2pi.
    absrad :: ℝ -> ℝ
    absrad rad
      | rad > (360*k) = rad `mod'` (360*k)
      | rad < 0       = absrad (360*k)+rad
      | otherwise     = rad

    -- now, if you passed through an axis, then the box must be expanded to include distance from axis of rotation in that direction.
    -- otherwise, put a box around the start and stop positions.

    noAxis :: Quadrant -> Quadrant -> Direction -> ℝ -> Box2
    noAxis q1 q2 dir amount
      | q1 == q2 && amount < 90*k && amount > -90*k = ((minX, minY), (maxX, maxY))
      | dir == Clockwise && q1 == UpperLeft  = oneAxis PosY q2 dir amount
      | dir == Clockwise && q1 == LowerRight = oneAxis PosX q2 dir amount
      | dir == Clockwise && q1 == LowerLeft  = oneAxis NegY q2 dir amount
      | dir == Clockwise && q1 == UpperRight = oneAxis NegX q2 dir amount
      | dir == CounterClockwise && q1 == UpperRight = oneAxis PosX q2 dir amount
      | dir == CounterClockwise && q1 == UpperLeft  = oneAxis PosY q2 dir amount
      | dir == CounterClockwise && q1 == LowerLeft  = oneAxis NegX q2 dir amount
      | dir == CounterClockwise && q1 == LowerRight = oneAxis NegY q2 dir amount
    noAxis _ _ _ _ = ((-distance, -distance), (distance, distance))
    oneAxis :: Axis -> Quadrant -> Direction -> ℝ -> Box2
    oneAxis axis quadrant dir amount
      | dir == Clockwise &&
        amount < 90*k && amount > -90*k &&
        ((axis == PosX && quadrant == LowerRight) ||
         (axis == NegY && quadrant == LowerLeft)  ||
         (axis == NegX && quadrant == UpperLeft)  ||
         (axis == PosY && quadrant == UpperRight))  = ((minX, minY), (maxX, maxY))
      | dir == CounterClockwise &&
        amount < 90*k && amount > -90*k &&
        ((axis == PosX && quadrant == UpperRight) ||
         (axis == PosY && quadrant == UpperLeft)  ||
         (axis == NegX && quadrant == LowerLeft)  ||
         (axis == NegY && quadrant == LowerRight))  = ((minX, minY), (maxX, maxY))
      | dir == Clockwise &&
        ((axis == PosX && quadrant == LowerLeft)  ||
         (axis == NegY && quadrant == UpperLeft)  ||
         (axis == NegX && quadrant == UpperRight) ||
         (axis == PosY && quadrant == LowerRight))  = crossOne axis dir
      | dir == CounterClockwise &&
        ((axis == PosX && quadrant == UpperLeft)  ||
         (axis == PosY && quadrant == LowerLeft)  ||
         (axis == NegX && quadrant == LowerRight) ||
         (axis == NegY && quadrant == UpperRight))  = crossOne axis dir
      | dir == Clockwise &&
        ((axis == PosX && quadrant == UpperLeft)  ||
         (axis == PosY && quadrant == LowerLeft)  ||
         (axis == NegX && quadrant == LowerRight) ||
         (axis == NegY && quadrant == UpperRight))  = crossTwo axis dir
      | dir == CounterClockwise &&
        ((axis == PosX && quadrant == LowerLeft)  ||
         (axis == NegY && quadrant == UpperLeft)  ||
         (axis == NegX && quadrant == UpperRight) ||
         (axis == PosY && quadrant == LowerRight))  = crossTwo axis dir
      | dir == Clockwise &&
        ((axis == PosX && quadrant == UpperRight) ||
         (axis == PosY && quadrant == UpperLeft)  ||
         (axis == NegX && quadrant == LowerLeft)  ||
         (axis == NegY && quadrant == LowerRight))  = crossThree axis
      | dir == CounterClockwise &&
        ((axis == PosX && quadrant == LowerRight) ||
         (axis == NegY && quadrant == LowerLeft)  ||
         (axis == NegX && quadrant == UpperLeft)  ||
         (axis == PosY && quadrant == UpperRight))  = crossThree axis
      | otherwise = ((-distance, -distance), (distance, distance))
    twoAxis :: Axis -> Axis -> Direction -> Box2
    twoAxis start stop dir
      | (start == PosX && stop == NegX) ||
        (start == PosY && stop == NegY) ||
        (start == NegX && stop == PosX) ||
        (start == NegY && stop == PosY)  = crossOne start dir
    twoAxis start stop dir
      | (start == PosX && stop == NegY) ||
        (start == NegY && stop == NegX) ||
        (start == NegX && stop == PosY) ||
        (start == PosY && stop == PosX)  = if dir == Clockwise
                                           then ((minX, minY), (maxX, maxY))
                                           else crossTwo start dir
      | (start == PosX && stop == PosY) ||
        (start == PosY && stop == NegX) ||
        (start == NegX && stop == NegY) ||
        (start == NegY && stop == PosX)  = if dir == CounterClockwise
                                           then ((minX, minY), (maxX, maxY))
                                           else crossTwo start dir
    twoAxis _ _ _ = ((-distance, -distance), (distance, distance))
    crossOne :: Axis -> Direction -> Box2
    crossOne start dir
      | (start == PosX && dir == Clockwise)        ||
        (start == NegX && dir == CounterClockwise)  = mixWith [(0,-distance)]
      | (start == NegY && dir == Clockwise)        ||
        (start == PosY && dir == CounterClockwise)  = mixWith [(-distance, 0)]
      | (start == NegX && dir == Clockwise)        ||
        (start == PosX && dir == CounterClockwise)  = mixWith [(0, distance)]
      | (start == PosY && dir == Clockwise)        ||
        (start == NegY && dir == CounterClockwise)  = mixWith [( distance, 0)]
      | otherwise = ((-distance, -distance), (distance, distance))
    crossTwo :: Axis -> Direction -> Box2
    crossTwo start dir
      | (start == PosX && dir == Clockwise)        ||
        (start == PosY && dir == CounterClockwise)  = mixWith [(-distance, 0), ( 0,-distance)]
      | (start == PosY && dir == Clockwise)        ||
        (start == NegX && dir == CounterClockwise)  = mixWith [( distance, 0), ( 0,-distance)]
      | (start == NegX && dir == Clockwise)        ||
        (start == NegY && dir == CounterClockwise)  = mixWith [( distance, 0), ( 0, distance)]
      | (start == NegY && dir == Clockwise)        ||
        (start == PosX && dir == CounterClockwise)  = mixWith [(-distance, 0), ( 0, distance)]
      | otherwise = ((-distance, -distance), (distance, distance))
    crossThree :: Axis -> Box2
    crossThree PosX = mixWith [( 0, distance), (-distance, 0), ( 0,-distance)]
    crossThree PosY = mixWith [(-distance, 0), ( 0,-distance), ( distance, 0)]
    crossThree NegX = mixWith [( 0,-distance), ( distance, 0), ( 0, distance)]
    crossThree NegY = mixWith [( distance, 0), ( 0, distance), (-distance, 0)]
    mixWith :: [ℝ2] -> Box2
    mixWith points = ((minimum xPoints, minimum yPoints), (maximum xPoints, maximum yPoints))
                     where
                       (xPoints, yPoints) = unzip $ points <> [(xStart, yStart), (xStop, yStop)]
    invertRotation :: Direction -> Direction
    invertRotation Clockwise = CounterClockwise
    invertRotation CounterClockwise = Clockwise
  in
    case rotationDirection of
      None -> ((xStart, yStart),(xStart, yStart))
      Rotation dir -> case rotationAmount of
                 amount | amount < 360*k && amount > -360*k ->
                          case startPosition of
                            CenterPoint -> emptyBox
                            OnAxis axis -> case stopPosition of
                                             OnAxis stopaxis         -> twoAxis axis stopaxis dir
                                             InQuadrant stopquadrant -> oneAxis axis stopquadrant dir amount
                                             CenterPoint -> emptyBox
                            InQuadrant quadrant -> case stopPosition of
                                             OnAxis stopaxis         -> oneAxis stopaxis quadrant (invertRotation dir) (-amount)
                                             InQuadrant stopquadrant -> noAxis quadrant stopquadrant dir travel
                                             CenterPoint -> emptyBox
                 _                         ->
                            ((-distance, -distance), (distance, distance))

