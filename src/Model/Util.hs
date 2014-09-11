{-# LANGUAGE MultiParamTypeClasses #-}

module Model.Util where

class Tuplify a b where
    toTuple   :: a -> b
    fromTuple :: b -> a
