{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Model.Trade where

import           Data.Int
import           Data.UUID
import           Database.Cassandra.CQL

import           Actor.Types
import           Model.Util

type TradeTuple = (UUID, UUID, UUID, Int64, Int64)

data Trade = Trade AskMerchantId BidMerchantId CommodityId Price Quantity deriving (Show)

instance Tuplify Trade TradeTuple where
    toTuple (Trade a b c d e) = ( unMerchantId a, unMerchantId b, unCommodityId c, unPrice d, unQuantity e)
    fromTuple (a, b, c, d, e) = (Trade (MerchantId a) (MerchantId b) (CommodityId c) (Price d) (Quantity e))

insertTrade :: Trade -> Cas ()
insertTrade u = executeWrite ONE q (toTuple u)
    where q :: Query Write TradeTuple ()
          q = "insert into trades (askMerchantId, bidMerchantId, commodityId, price, quantity) values (?, ?, ?, ?, ?)"
