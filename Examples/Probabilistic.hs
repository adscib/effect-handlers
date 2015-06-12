{-# LANGUAGE TypeFamilies,
    GADTs,
    RankNTypes,
    MultiParamTypeClasses,
    QuasiQuotes,
    FlexibleInstances,
    FlexibleContexts,
    OverlappingInstances,
    UndecidableInstances,
    ConstraintKinds #-}

import Control.Monad
import Control.Arrow (first, second)
import System.Random
import Handlers
import DesugarHandlers

pick :: [(a,Double)] -> Double -> a
-- | Picks a value from a distribution using a real from [0,1].
pick ((x,p):ps) v = if v <= p then x else pick ps (v-p)
pick []         v = error $ "Sampling failed. Missing probability mass "
                    ++ show v

data Dist a = Dist [(a,Double)]

sample :: StdGen -> Dist a -> (a,StdGen)
pdf    :: Eq a => Dist a -> a -> Double

sample g (Dist xs) = first (pick xs) $ randomR (0.0,1.0) g
pdf (Dist xs) x =
  case lookup x xs of
    Just p -> p
    Nothing -> 0
exact (Dist xs) = xs
score :: Double -> Dist a -> Dist a
score p (Dist xs) = Dist $ map (second (*p)) xs

instance Monad Dist where
  return x = Dist [(x,1)]
  (Dist xs) >>= f = Dist [(y,p*q) | (x,p) <- xs, (y,q) <- exact (f x)]

[operation|forall a.Draw    :: Dist a -> a      |]
[operation|forall a.Observe :: Dist Int -> Int -> ()|]

[operation|Reject :: () |]

[operation|Weight :: Double -> ()|]
[operation|Barrier :: ()|]

[handler|
  Enumerate a :: Dist a handles {Draw,Weight} where
    Return x   -> return x
    Draw d k   -> d >>= k
    Weight p k -> score p (k ())
|]


[handler|
  Run a :: StdGen -> a handles {Draw} where
    Return x   g -> x
    Draw   d k g -> let (x,g') = sample g d in k x g' 
|]

[handler|
  forward h handles {Reject, Draw}.
    Rejection a :: a handles {Observe} where
      Return  x     -> return x
      Observe d x k -> 
        do {y <- draw d; if x == y then k () else reject >>= k;}
|]

--repeat conflicts with Prelude
[handler|
   forward h.
     Repeat' a :: Comp (Repeat' h a) a -> a handles {Reject} where
       Return x   _ -> return x
       Reject   k c -> repeat' c c
|]

[handler|
  forward h handles {Weight}.
    Weigh a :: a handles {Observe} where
      Return  x     -> return x
      Observe d x k -> weight (pdf d x) >>= k
|]

[handler|
  forward h.
    Importance a :: Double -> (a,Double) handles {Weight} where
      Return x   w -> return (x,w)
      Weight p k w -> k () (p*w)
|]

[handler|
  forward h.
    Prior a :: a handles {Observe, Weight} where
      Return  x     -> x
      Observe _ _ k -> k ()
      Weight  _ k   -> k ()
|]

-- [handler|
--   forward h.
--     Mh a :: Comp (Persevere h a) a -> [a] handles {Weight} where
--       Return x   c -> x : mh c
--       Weight p k c -> 

-- [handler|
--   forward h.
--     Smc a :: [(a,Double)] -> [(a,Double)] handles {Weight} where
--       Return x   ps -> return ps
--       Weight p k ps -> 
  

die = Dist $ map (\x -> (x,1/6)) [1..6] :: Dist Int
type Prob a = forall h.([handles|h {Draw}    |],
                        [handles|h {Observe} |]) => Comp h a
example :: Prob Int
example = do
  x <- draw die
  y <- draw die
  let z = x + y
  observe (return z) 3
  return x

solution :: Dist Int
solution = (enumerate . weigh) example

sampler :: StdGen -> Int
sampler g = run g $ repeat' d d where
  d = rejection example

g = mkStdGen 0

main = undefined
