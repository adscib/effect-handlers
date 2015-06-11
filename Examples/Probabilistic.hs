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
import System.Random
import Handlers
import DesugarHandlers

data Dist a = Dist

sample :: StdGen -> Dist a -> (a,StdGen)
pdf    :: Dist a -> a -> Double

sample = undefined
pdf = undefined

[operation|Draw    a :: Dist a -> a      |]
[operation|Observe a :: Dist a -> a -> ()|]

[operation|Reject :: () |]

[operation|Weight :: Double -> ()|]
[operation|Barrier :: ()|]


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
  



main = undefined
