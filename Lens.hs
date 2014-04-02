{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
module Lens where
import Control.Eff              (Eff, Member)
import Control.Eff.State.Strict
import Control.Lens             (ASetter, Profunctor, Setting, set, (%~), (*~))
import Control.Lens             ((+~), (-~), (.~), (?~))
import Control.Lens             ((//~))
import Control.Lens             ((^~))
import Control.Lens             ((^^~))
import Control.Lens             ((**~))
import Control.Lens             ((&&~))
import Control.Lens             ((||~))
import Data.Typeable            (Typeable)

infix  4 .=, +=, *=, -=, //=, ^=, ^^=, **=, &&=, ||=, %=, ?=
infixr 2 <~

assign :: (Typeable s, Member (State s) r) => ASetter s s a b -> b -> Eff r ()
assign l b = modify $ set l b
{-# INLINE assign #-}

(.=) :: (Typeable s, Member (State s) r) => ASetter s s a b -> b -> Eff r ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

(%=) :: (Typeable s, Profunctor p, Member (State s) r) => Setting p s s a b -> p a b -> Eff r ()
l %= f = modify (l %~ f)
{-# INLINE (%=) #-}

(?=) :: (Typeable s, Member (State s) r) => ASetter s s a (Maybe b) -> b -> Eff r ()
l ?= b = modify (l ?~ b)
{-# INLINE (?=) #-}

(+=) :: (Num a, Typeable s, Member (State s) r) => ASetter s s a a -> a -> Eff r ()
l += b = modify (l +~ b)
{-# INLINE (+=) #-}

(-=) :: (Num a, Typeable s, Member (State s) r) => ASetter s s a a -> a -> Eff r ()
l -= b = modify (l -~ b)
{-# INLINE (-=) #-}

(*=) :: (Num a, Typeable s, Member (State s) r) => ASetter s s a a -> a -> Eff r ()
l *= b = modify (l *~ b)
{-# INLINE (*=) #-}

(//=) :: (Fractional a, Typeable s, Member (State s) r) => ASetter s s a a -> a -> Eff r ()
l //= b = modify (l //~ b)
{-# INLINE (//=) #-}

(^=) :: (Integral e, Num a, Typeable s, Member (State s) r) => ASetter s s a a -> e -> Eff r ()
l ^= b = modify (l ^~ b)
{-# INLINE (^=) #-}

(^^=) :: (Fractional a, Integral e, Typeable s, Member (State s) r) => ASetter s s a a -> e -> Eff r ()
l ^^= b = modify (l ^^~ b)
{-# INLINE (^^=) #-}

(**=) :: (Floating a, Typeable s, Member (State s) r) => ASetter s s a a -> a -> Eff r ()
l **= b = modify (l **~ b)
{-# INLINE (**=) #-}

(&&=) :: (Typeable s, Member (State s) r) => ASetter s s Bool Bool -> Bool -> Eff r ()
l &&= b = modify (l &&~ b)
{-# INLINE (&&=) #-}

(||=) :: (Typeable s, Member (State s) r) => ASetter s s Bool Bool -> Bool -> Eff r ()
l ||= b = modify (l ||~ b)
{-# INLINE (||=) #-}

(<~) :: (Typeable s, Member (State s) r) => ASetter s s a1 a -> Eff r a -> Eff r ()
l <~ mb = mb >>= (l .=)
{-# INLINE (<~) #-}
