{-|

Finding all "uncrossed knight's paths" 
(http://en.wikipedia.org/wiki/Longest_uncrossed_knight%27s_path) on a
NxM rectangular board. 

-}
module UncrossedPaths
       (
         possiblePaths
       , StepData(..)
       , Limits(..)
       , StepWrapper(..)
       , Path
       ) where


{-|
Find recursively all paths that reach the starting point without
crossing themselves.
-}
possiblePaths :: Limits -> Path -> StepWrapper -> [Path]
possiblePaths limits pathSoFar candidate
  | noStepsYet = continuePath
  | outOfBounds = deadEnd
  | alreadyVisited = deadEnd
  | crossesStep pathSoFar candidate = deadEnd
  | wrongDirection = deadEnd
  | finished = [candidateAccepted]
  | otherwise = continuePath
  where 
    noStepsYet = null pathSoFar
    outOfBounds = not $ (Limits 0 0)  |<| (p candidate) 
                     && (p candidate) |<| limits
    points = map p pathSoFar
    alreadyVisited = (p candidate) `elem` (tail points)
    wrongDirection = (y (p candidate)) == 0
                     && not (0 `elem` (map x points))
    finished = (p candidate) == head points
    deadEnd = [[]]
    candidateAccepted = pathSoFar ++ [candidate]
    continuePath = filter (not . null) $ 
                   nextSteps candidate >>= 
                   possiblePaths limits candidateAccepted

{-|
Check if a candidate step crosses the path so far.
-}
crossesStep :: Path -> StepWrapper -> Bool
crossesStep (first:[]) candidate = False
crossesStep (first:second:[]) candidate = False
crossesStep (first:pathSoFar) candidate =
  let path = if (p candidate) == (p first)
             then tail (init pathSoFar)
             else (init pathSoFar)
  in any id $ map crossing path
  where
    crossing sw = sumPointsClose (sp sw) (sp candidate)
                  && not (parallelSteps (s sw) (s candidate))

{-|
All eight knight's steps from a point.
-}
nextSteps :: StepWrapper -> [StepWrapper]
nextSteps sw = [let step = Step a b
                    oldP = p sw
                    newP = oldP <+> step 
                in StepWrapper newP step (oldP <+> newP) 
               | a <- [-1, 1, -2, 2], b <- [-1, 1, -2, 2], abs a /= abs b]

{-|
Data that is collected on each step.  'SumPoint' is supposed to be
the sum of current and previous points and is used to identify
crossing steps.
-} 
data StepData = Point {x :: Int, y :: Int} 
              | Step  {dx :: Int, dy :: Int} 
              | SumPoint {x :: Int, y :: Int} 
              deriving (Eq, Show)

data Limits = Limits {xx  :: Int, yy :: Int} 
            deriving (Eq, Show)

data StepWrapper = StepWrapper { p :: StepData, 
                                 s :: StepData,
                                sp :: StepData}
                 deriving (Eq, Show)
                                    
type Path = [StepWrapper]

-- | "Addition" operator
(<+>) ::  StepData -> StepData -> StepData
(Point x  y ) <+> (Step  dx dy) = Point (x  + dx) (y  + dy) 
(Point x1 y1) <+> (Point x2 y2) = SumPoint (x1 + x2) (y1 + y2) 
_ <+> _ = undefined
            
-- | "Subtraction" operator 
(<->) :: StepData -> StepData -> StepData
(Point x1 y1) <-> (Point x2 y2) = Step (x1 - x2) (y1 - y2)
_ <-> _ = undefined

-- | The opposite of a step
neg :: StepData -> StepData
neg (Step dx dy) = Step (-dx) (-dy)
neg _ = undefined
                                  
class Pairs a where
  toTuple :: a -> (Int, Int)
  
instance Pairs StepData where
  toTuple (Point x y)  = (x, y)
  toTuple (Step dx dy) = (dx, dy)
  toTuple (SumPoint x y)  = (x, y)
  
instance Pairs Limits where
  toTuple (Limits a b) = (a, b)
  
-- | Operator to determine a Point is not out-of-bounds
(|<|) :: (Pairs a, Pairs b) => a -> b -> Bool
(|<|) first second = let (x1, y1) = toTuple first
                         (x2, y2) = toTuple second
                     in if x1 <= x2 && y1 <= y2
                        then True
                        else False

parallelSteps :: StepData -> StepData -> Bool
parallelSteps s1 s2 = s1 == s2 || s1 == (neg s2)

{-|
If two sums of two consecutive points are close, it is possible  
the corresponding steps are crossing.  This is the same as testing
the closeness of the average of two consencutive points.
-}
sumPointsClose :: StepData -> StepData -> Bool
sumPointsClose sp1 sp2 = ((y sp2) - (y sp1))^2 
                         + ((x sp2) - (x sp1))^2 <= 4
                         
                         