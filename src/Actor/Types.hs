{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Actor.Types
    ( OrderId (..)
    , Price (..)
    , Quantity (..)
    ) where

import           Data.Binary   (Binary)
import           Data.Data
import           Data.Typeable
import           Data.UUID
import           GHC.Int

newtype OrderId  = OrderId { unOrderId :: UUID }    deriving (Eq, Ord, Typeable, Data, Binary)
newtype Price    = Price   { unPrice :: Int64 }     deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary)
newtype Quantity = Quantity { unQuantity :: Int32 } deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary)

