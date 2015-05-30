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

[operation|Flip   :: Bool|]
[operation|Weight :: Double -> ()|]
[operation|Barrier :: ()|]

[handler|
  Sample a :: StdGen -> a handles {Flip} where
    Return x g -> x
    Flip   k g -> let (b,g') = random g in k b g' 
|]

[handler|
  forward h.
    Prior a :: a handles {Weight} where
      Return x -> x
      Weight _ k -> k ()
|]

[handler|
  forward h.
    PriorScore a :: Double -> (a,Double) handles {Weight} where
      Return x   w -> return (x,w)
      Weight p k w -> k () (p*w)
|]

  



main = undefined
