{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Promotions where

import Data.Aeson (FromJSON, ToJSON, Value (String), toJSON)
import Data.Aeson.Types (Value)
import qualified Data.Set as S
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Campaign = FreeShippingCampaign
  { startDate :: UTCTime,
    endDate :: UTCTime,
    minTotalPrice :: Money
  }
  deriving (Eq, Show)

data Promotion = FreeShipping deriving (Eq, Show, Generic)

instance ToJSON Promotion where
  toJSON FreeShipping = String "free_shipping"

newtype Money = Money Double
  deriving (Eq, Show)
  deriving (FromJSON, Ord) via Double

newtype Quantity = Quantity Int
  deriving (Eq, Show)
  deriving (FromJSON) via Int

newtype ProductId = ProductId Int
  deriving (Eq, Show)
  deriving (FromJSON) via Int

data CartItem = CartItem
  { productId :: ProductId,
    quantity :: Quantity
  }
  deriving (Eq, Show, Generic)

instance FromJSON CartItem

data PromotionRequest = PromotionRequest
  { shippingCost :: Money,
    totalPrice :: Money,
    items :: [CartItem]
  }
  deriving (Eq, Show, Generic)

data PromotionResponse = PromotionResponse
  { promotions :: S.Set Promotion
  }
  deriving (Eq, Show, Generic)

instance FromJSON PromotionRequest

instance ToJSON PromotionResponse
