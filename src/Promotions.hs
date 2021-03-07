module Promotions where

data ProductId = ProductId String deriving (Eq, Show)
data Price = Price Double deriving (Eq, Show)
data SellerId = SellerId String deriving (Eq, Show)
data CartItemId = CartItemId String deriving (Eq, Show)

data Product = Product {
  id :: ProductId,
  price :: Price,
  sellerId :: SellerId
} deriving (Eq, Show)

data CartItem1 = CartItem1 Product Int deriving (Eq, Show)

data CartItem2 = CartItem2 CartItemId Product deriving (Eq, Show)

data Cart1 = Cart1 [CartItem1] deriving (Eq, Show)
data Cart2 = Cart2 [CartItem2] deriving (Eq, Show)

data Promotion = FreeProduct CartItemId | FreeShipping | PercentageOff CartItemId Double | TotalDiscount Price

data Campaign = Campaign

f :: Cart2 -> Campaign -> [Promotion]
f _ _ = [] -- just so it builds

bestCampaign :: Campaign -> Campaign -> Campaign
bestCampaign c1 _ = c1 -- just so it builds
