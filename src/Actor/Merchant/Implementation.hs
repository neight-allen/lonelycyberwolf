module Actor.Merchant.Implementation
    ( Inventory (..)
    , merchant
    ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Platform.ManagedProcess

import           Actor.Merchant
import           Actor.Types

data Inventory = Inventory

----

merchant :: ProcessDefinition Inventory
merchant = ProcessDefinition
        { apiHandlers            = []
        , infoHandlers           = []
        , exitHandlers           = []
        , timeoutHandler         = \s _ -> continue s
        , shutdownHandler        = \_ _ -> return ()
        , unhandledMessagePolicy = Drop
        }
