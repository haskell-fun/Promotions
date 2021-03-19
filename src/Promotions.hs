
module Promotions where

import qualified Data.Set as S
import Data.Semigroup
import Data.Monoid

data ProductId = ProductId String deriving (Eq, Show)
data Price = Price Double deriving (Eq, Show)
data SellerId = SellerId String deriving (Eq, Show)
data CartItemId = CartItemId String deriving (Eq, Show)
data ShippingCost = ShippingCost Double deriving (Eq, Show)

data Item = Item {
  id :: ProductId,
  price :: Price,
  sellerId :: SellerId
} deriving (Eq, Show)

data CartItem = CartItem CartItemId Item deriving (Eq, Show)

data Cart = Cart [CartItem] ShippingCost deriving (Eq, Show)

data Promotion = FreeProduct CartItemId | FreeShipping | PercentageOff CartItemId Double | TotalDiscount Price

data Campaign = Campaign | NoOpCampaign

f2 :: Cart -> [Campaign] -> S.Set Promotion
f2 cart = (foldr (<>) S.empty) . (fmap (f cart))

f :: Cart -> Campaign -> S.Set Promotion
f _ _ = S.empty -- just so it builds

bestCampaign :: Campaign -> Campaign -> Campaign
bestCampaign c1 _ = c1 -- just so it builds

class Equality a where
  isEqual :: a -> a -> Bool

class (Eq a) => Equality a where
  isEqual x y = x == y

-- instance Equality Bool where
--   isEqual :: Bool -> Bool -> Bool
--   isEqual True True = True
--   isEqual False False = True
--   isEqual _ _ = False

-- instance Monoid SumInt where
--   mempty = SumInt 0
--
-- instance Monoid ProductInt where
--   mempty = ProductInt 1
--
-- instance Semigroup SumInt where
--   (SumInt a) <> (SumInt b) = SumInt (a + b)

-- instance Semigroup ProductInt where
--   (ProductInt a) <> (ProductInt b) = ProductInt (a * b)

-- data SumInt = SumInt Int
-- data ProductInt = ProductInt Int deriving (Semigroup, Monoid) via Int
