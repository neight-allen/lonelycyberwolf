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
    arbitrary = ConductorId <$> choose (nil, nil)

instance Arbitrary ClerkId where
    arbitrary = ClerkId <$> choose (nil, nil)

instance Arbitrary MerchantId where
    arbitrary = MerchantId <$> choose (nil, nil)

instance Arbitrary EscrowId where
    arbitrary = EscrowId <$> choose (nil, nil)

--

instance Arbitrary ConductorPid where
    arbitrary = ConductorPid <$> arbitrary

instance Arbitrary ClerkPid where
    arbitrary = ClerkPid <$> arbitrary

instance Arbitrary MerchantPid where
    arbitrary = MerchantPid <$> arbitrary

instance Arbitrary EscrowPid where
    arbitrary = EscrowPid <$> arbitrary

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

instance Arbitrary Price where
    arbitrary = choose (1, maxBound)

instance Arbitrary Quantity where
    arbitrary = choose (1, maxBound)
