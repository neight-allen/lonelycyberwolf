module Actor.Types where

import           Control.Applicative
import           Control.Distributed.Process                hiding (Match)
import           Control.Distributed.Process.Internal.Types
import           Data.Binary                                (Binary)
import qualified Data.ByteString                            as BS
import           Data.Data                                  hiding (Proxy)
import           Data.IxSet
import           Data.Text
import           Data.Typeable                              hiding (Proxy)
import           Data.UUID
import           GHC.Generics
import           GHC.Int
import           System.Random

--

newtype ConductorPid = ConductorPid   { unConductorPid :: ProcessId }  deriving (Eq, Ord, Typeable, Data, Binary, Show)
newtype ClerkPid     = ClerkPid       { unClerkPid :: ProcessId }      deriving (Eq, Ord, Typeable, Data, Binary, Show)
newtype MerchantPid  = MerchantPid    { unMerchantPid :: ProcessId }   deriving (Eq, Ord, Typeable, Data, Binary, Show)
newtype EscrowPid    = EscrowPid      { unEscrowPid :: ProcessId }     deriving (Eq, Ord, Typeable, Data, Binary, Show)

--

newtype OrderId  = OrderId  { unOrderId :: UUID }   deriving (Eq, Ord, Typeable, Data, Binary, Show, Random)
newtype Price    = Price    { unPrice :: Int64 }    deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary, Show, Random)
newtype Quantity = Quantity { unQuantity :: Int64 } deriving (Eq, Ord, Num, Real, Integral, Enum, Typeable, Data, Binary, Show, Random)

instance Bounded Price where
    minBound = 0
    maxBound = Price maxBound

instance Bounded Quantity where
    minBound = 0
    maxBound = Quantity maxBound

--

data Order = Order OrderId MerchantPid Price Quantity deriving (Show, Typeable, Data)

instance Eq Order where
    (Order id0 _ _ _) == (Order id1 _ _ _) = id0 == id1

instance Ord Order where
    compare (Order _ _ p0 _) (Order _ _ p1 _) = compare p0 p1

instance Indexable Order where
    empty = ixSet [ ixGen (Proxy :: Proxy OrderId)
                  , ixGen (Proxy :: Proxy Price)
                  ]

data OrderBook = OrderBook
        { askBook :: !(IxSet Order)
        , bidBook :: !(IxSet Order)
        } deriving (Show)

--

type AskId = OrderId
type BidId = OrderId

type AskMerchant = MerchantPid
type BidMerchant = MerchantPid

type Ask = Order
type Bid = Order

data Match = Match AskId AskMerchant BidId BidMerchant Price Quantity deriving (Eq, Generic, Typeable)

instance Ord Match where
    compare (Match _ _ _ _ p0 _) (Match _ _ _ _ p1 _) = compare p0 p1

instance Binary Match

--

type Name        = Text
type Description = Text

newtype CommodityId = CommdityId { unCommodityId :: UUID } deriving (Eq, Ord, Typeable, Data, Binary, Show, Random)
data Commodity = Commodity CommodityId Name Description deriving (Typeable, Data, Generic, Show)

instance Eq Commodity where
    (Commodity id0 _ _) == (Commodity id1 _ _) = id0 == id1

instance Ord Commodity where
    compare (Commodity _ n0 _) (Commodity _ n1 _) = compare n0 n1
