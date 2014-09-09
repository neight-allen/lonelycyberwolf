module Actor.Escrow.Implementation where

import           Control.Distributed.Process                         hiding
                                                                      (Match)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Data.Data                                           hiding
                                                                      (Proxy)
import           Data.IxSet                                          hiding
                                                                      (Proxy)
import           Data.Typeable
import           GHC.Generics

import           Actor.Escrow
import           Actor.Merchant
import           Actor.Types

newtype AskIdK = AskIdK { unAskIdK :: OrderId } deriving (Eq, Ord, Typeable, Data)
newtype BidIdK = BidIdK { unBidIdK :: OrderId } deriving (Eq, Ord, Typeable, Data)

data Trade = Trade AskId BidId Match deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Indexable Trade where
    empty = ixSet [ ixFun (\(Trade aid _ _) -> [AskIdK aid])
                  , ixFun (\(Trade _ bid _) -> [BidIdK bid])
                  ]

type OpenTrades = IxSet Trade

data Escrow = Escrow EscrowId OpenTrades deriving (Show)

escrow :: ProcessDefinition Escrow
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

commitAsk' :: Escrow -> CommitAsk -> Process (ProcessReply () Escrow)
commitAsk' (Escrow eid ot) (CommitAsk (Match aid am bid bm p q)) =
        case toList $ ot @= AskIdK aid of
            []  -> noReply_ $ Escrow eid (insert (Trade aid bid (Match aid am bid bm p q)) ot)
            [t@(Trade aid' bid' (Match _ am' _ bm' p' q'))] -> do
                -- Perform db log
                confirmTransaction am' (Match aid am bid bm p q)
                confirmTransaction bm' (Match aid am bid bm p q)
                noReply_ $ Escrow eid (delete t ot)

commitBid' :: Escrow -> CommitBid -> Process (ProcessReply () Escrow)
commitBid' (Escrow eid ot) (CommitBid (Match aid am bid bm p q)) =
        case toList $ ot @= BidIdK bid of
            [] -> noReply_ $ Escrow eid (insert (Trade aid bid (Match aid am bid bm p q)) ot)
            [t@(Trade aid' bid' (Match _ am' _ bm' p' q'))] -> do
                -- Perform db log
                confirmTransaction am' (Match aid am bid bm p q)
                confirmTransaction bm' (Match aid am bid bm p q)
                noReply_ $ Escrow eid (delete t ot)
