{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Actor.Types.Arbitrary where

import           Control.Applicative
import           Control.Distributed.Process
import           Control.Distributed.Process.Internal.Types
import qualified Data.ByteString                            as BS
import           Data.UUID
import           Network.Transport
import           Test.Tasty.QuickCheck

import           Actor.Types

instance Arbitrary ConductorId where
    arbitrary = ConductorId <$> arbitrary

instance Arbitrary ClerkId where
    arbitrary = ClerkId <$> arbitrary

instance Arbitrary MerchantId where
    arbitrary = MerchantId <$> arbitrary

instance Arbitrary EscrowId where
    arbitrary = EscrowId <$> arbitrary

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

instance Arbitrary Price where
    arbitrary = choose (1, maxBound)

instance Arbitrary Quantity where
    arbitrary = choose (1, maxBound)
