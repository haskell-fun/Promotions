{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad (mfilter)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON)
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Promotions (Campaign (endDate, minTotalPrice, startDate), Promotion (FreeShipping), PromotionRequest (totalPrice), PromotionResponse (..))
import Servant (Application, JSON, Post, ReqBody, serve, (:<|>) (..), (:>))
import Servant.API (Get)

startServer :: IO ()
startServer = run 8080 application

type GetStatus = "status" :> Get '[JSON] StatusResponse

type PromotionRpc = "api" :> "v1" :> "promotion" :> ReqBody '[JSON] PromotionRequest :> Post '[JSON] PromotionResponse

newtype StatusResponse = StatusResponse
  { healthy :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON StatusResponse

type Endpoints = GetStatus :<|> PromotionRpc

endpoints :: Proxy Endpoints
endpoints = Proxy

application :: Application
application = serve endpoints $ status :<|> calculatePromotions

status :: Monad m => m StatusResponse
status = pure $ StatusResponse "ok"

calculatePromotions :: MonadIO m => PromotionRequest -> m PromotionResponse
calculatePromotions r = liftIO $ applyCampaignMaybe r <$> fetchActiveCampaigns

fetchCampaign :: IO (Maybe Campaign)
fetchCampaign = pure Nothing -- TODO read from a config file

fetchActiveCampaigns :: IO (Maybe Campaign)
fetchActiveCampaigns = filterActiveCampaign <$> getCurrentTime <*> fetchCampaign

filterActiveCampaign :: UTCTime -> Maybe Campaign -> Maybe Campaign
filterActiveCampaign now = mfilter $ isCampaignActive now

isCampaignActive :: UTCTime -> Campaign -> Bool
isCampaignActive now c = startDate c < now && endDate c > now

applyCampaignMaybe :: PromotionRequest -> Maybe Campaign -> PromotionResponse
applyCampaignMaybe r = PromotionResponse . fromMaybe S.empty . fmap (applyCampaign r)

applyCampaign :: PromotionRequest -> Campaign -> S.Set Promotion
applyCampaign r c
  | totalPrice r > minTotalPrice c = S.singleton FreeShipping
  | otherwise = S.empty
