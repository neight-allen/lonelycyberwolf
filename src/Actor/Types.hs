module Actor.Types
    ( ConductorId (..)
    , ClerkId (..)
    , MerchantId (..)

    , OrderId (..)
    , Price (..)
    , Quantity (..)
    ) where

import           Control.Distributed.Process
import           Data.Binary                 (Binary)
import           Data.Data
import           Data.Typeable
import           Data.UUID
import           GHC.Int

-- Process Types --

newtype ConductorId = ConductorId   { unConductorId :: ProcessId }  deriving (Eq, Ord, Typeable, Data, Binary)
newtype ClerkId     = ClerkId       { unClerkId :: ProcessId }      deriving (Eq, Ord, Typeable, Data, Binary)
newtype MerchantId  = MerchantId    { unMerchantId :: ProcessId }   deriving (Eq, Ord, Typeable, Data, Binary)

--

newtype OrderId  = OrderId  { unOrderId :: UUID }   deriving (Eq, Ord, Typeable, Data, Binary)
newtype Price    = Price    { unPrice :: Int64 }    deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary)
newtype Quantity = Quantity { unQuantity :: Int32 } deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary)
