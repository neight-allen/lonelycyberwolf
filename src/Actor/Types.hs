module Actor.Types
    ( ConductorId (..)
    , ClerkId (..)
    , MerchantId (..)

    , OrderId (..)
    , Price (..)
    , Quantity (..)
    ) where

import           Control.Applicative
import           Control.Distributed.Process
import           Control.Distributed.Process.Internal.Types
import           Data.Binary                                (Binary)
import qualified Data.ByteString                            as BS
import           Data.Data
import           Data.Typeable
import           Data.UUID
import           GHC.Int
import           Network.Transport
import           System.Random
import           Test.Tasty.QuickCheck

-- Process Types --

newtype ConductorId = ConductorId   { unConductorId :: ProcessId }  deriving (Eq, Ord, Typeable, Data, Binary, Show, Arbitrary)
newtype ClerkId     = ClerkId       { unClerkId :: ProcessId }      deriving (Eq, Ord, Typeable, Data, Binary, Show, Arbitrary)
newtype MerchantId  = MerchantId    { unMerchantId :: ProcessId }   deriving (Eq, Ord, Typeable, Data, Binary, Show, Arbitrary)

--

newtype OrderId  = OrderId  { unOrderId :: UUID }   deriving (Eq, Ord, Typeable, Data, Binary, Show, Random)
newtype Price    = Price    { unPrice :: Int64 }    deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary, Show, Arbitrary, Bounded, Random)
newtype Quantity = Quantity { unQuantity :: Int64 } deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary, Show, Arbitrary, Bounded, Random)

--

instance Arbitrary OrderId where
    arbitrary = OrderId <$> choose (nil, nil)

instance Arbitrary ProcessId where
    arbitrary = ProcessId <$> arbitrary <*> arbitrary

instance Arbitrary NodeId where
    arbitrary = NodeId <$> arbitrary

instance Arbitrary EndPointAddress where
    arbitrary = EndPointAddress . BS.pack <$> vector 4

instance Arbitrary LocalProcessId where
    arbitrary = LocalProcessId <$> arbitrary <*> arbitrary
