module Actor.Escrow.Implementation
    ( escrow
    ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Platform.ManagedProcess

import           Actor.Escrow
import           Actor.Types

data Holding = Holding deriving (Show)

escrow :: ProcessDefinition Holding
escrow = ProcessDefinition
        { apiHandlers            = [ handleCall commitAsk'
                                   , handleCall commitBid'
                                   ]
        , infoHandlers           = []
        , exitHandlers           = []
        , timeoutHandler         = \s _ -> continue s
        , shutdownHandler        = \_ _ -> return ()
        , unhandledMessagePolicy = Drop
        }

commitAsk' :: Holding -> CommitAsk -> Process (ProcessReply () Holding)
commitAsk' = undefined

commitBid' :: Holding -> CommitBid -> Process (ProcessReply () Holding)
commitBid' = undefined
