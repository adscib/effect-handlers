{- Shallow handlers for indexed types using a free monad -}

{-# LANGUAGE
    GADTs,
    TypeFamilies,
    MultiParamTypeClasses,
    FlexibleInstances,
    FlexibleContexts,
    TypeOperators, 
    PolyKinds, DataKinds, RankNTypes, ScopedTypeVariables
  #-}

module ShallowFreeHandlerIx where

import Control.Exception (catch, IOException)
import System.IO

import FunctorIx
import MonadIx

type family Return (op :: k -> *) :: k -> *

type family Result (h :: k -> *) :: k -> *
type family Inner  (h :: k -> *) :: *
type family Post   (h :: k -> *) :: k

class (((h :: k -> *) `Handles` (op :: k -> *))) where
  clause :: op pre -> (Return op :-> Comp h (Inner h := Post h)) -> h pre -> Result h pre

data Comp (h :: k -> *) (a :: (k -> *)) :: (k -> *) where
  Ret :: a pre -> Comp h a pre
  Do  :: (h `Handles` op) => op pre -> (Return op :-> Comp h a) -> Comp h a pre

instance MonadIx (Comp h) where
  returnIx             = Ret
  extendIx f (Ret v)   = f v
  extendIx f (Do op k) = Do op (\x -> k x ?>= f)
instance FunctorIx (Comp h) where
  mapIx f c = c ?>= (returnIx . f)
instance ApplicativeP (Comp h) where
  pure      = returnP
  mf |*| ms = mf =>= \f -> ms =>= \s -> returnP (f s)

doOp :: (h `Handles` op) => op post -> Comp h (Return op) post
doOp op = Do op returnIx

handle :: Comp h (Inner h := Post h) pre -> (Inner h -> h pre -> Result h pre) -> h pre -> Result h pre
handle (Ret (V x)) r h = r x h
handle (Do op k)   r h = clause op k h

handleInner :: Comp h (Inner h := Post h) pre -> h pre -> Result h pre
handleInner (Do op k) h = clause op k h

-- Example from Kleisli arrows of outrageous fortune
data FileState :: * where
  Open   :: FileState
  Closed :: FileState

data SFileState :: FileState -> * where
  SOpen   :: SFileState 'Open
  SClosed :: SFileState 'Closed

data FOpen i = FOpen ((FilePath := Closed) i)
type instance Return FOpen = SFileState
fOpen :: (h `Handles` FOpen) => String -> Comp h SFileState 'Closed
fOpen p = doOp (FOpen (V p))

data FGetC i = FGetC ((() := 'Open) i)
type instance Return FGetC = Maybe Char := 'Open
fGetC :: (h `Handles` FGetC) => Comp h (Maybe Char := 'Open) 'Open
fGetC = doOp (FGetC (V ()))

data FClose i = FClose ((() := 'Open) i)
type instance Return FClose = () := 'Closed
fClose :: (h `Handles` FClose) => Comp h (() := 'Closed) 'Open
fClose = doOp (FClose (V ()))

fileContents :: (h `Handles` FOpen, h `Handles` FGetC, h `Handles` FClose) =>
                  String -> Comp h (Maybe String := 'Closed) 'Closed
fileContents p = fOpen p ?>= \b -> case b of
  SClosed -> pure Nothing
  SOpen   -> pure (\s _ -> Just s) |*| readOpenFile |*| fClose

readOpenFile :: (h `Handles` FGetC) => Comp h (String := 'Open) 'Open
readOpenFile = fGetC =>= \x -> case x of
  Nothing -> pure ""
  Just c  -> pure (c:) |*| readOpenFile

newtype Wrap (a :: *) (i :: k) = Wrap {unWrap :: IO a}

data FH (a :: *) (pre :: k) where
     ClosedFH ::           FH a 'Closed 
     OpenFH   :: Handle -> FH a 'Open
type instance Result (FH a) = Wrap a
type instance Inner (FH a)  = a
type instance Post (FH a)   = 'Closed
                               
instance (FH a `Handles` FOpen) where
  clause (FOpen (V s)) k ClosedFH =
          Wrap
          (catch
           (openFile s ReadMode >>= \h -> unWrap (openFH h (k SOpen)))
           (\(_ :: IOException) -> unWrap (runFH (k SClosed))))
runFH :: Comp (FH a) (a := 'Closed) 'Closed -> Wrap a 'Closed
runFH m = handle m (\x _ -> Wrap (return x)) ClosedFH

instance (FH a `Handles` FClose) where
  clause (FClose (V ())) k (OpenFH file) = Wrap (hClose file >> unWrap (runFH (k (V ()))))
instance (FH a `Handles` FGetC) where
  clause (FGetC (V ())) k (OpenFH file) =
          Wrap
            (catch
              (hGetChar file >>= \c -> unWrap (openFH file (k (V (Just c)))))
              (\(_ :: IOException) -> unWrap (openFH file (k (V Nothing)))))
openFH :: Handle -> Comp (FH a) (a := 'Closed) 'Open -> Wrap a 'Open
openFH file m = handleInner m (OpenFH file)

test1 = unWrap (runFH (fileContents "test.txt"))
test2 = unWrap (runFH (fileContents "HandlerIx.hs"))
