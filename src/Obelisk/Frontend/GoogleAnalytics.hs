{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Obelisk.Frontend.GoogleAnalytics
  ( googleAnalyticsFromConfig
  , googleAnalytics
  , trackingIdPath
  , getTrackingId
  ) where

import Control.Monad (void)
import Data.Text (Text)
import GHCJS.DOM.Types (liftJSM)
import Language.Javascript.JSaddle.Evaluate (eval)
import Obelisk.Configs
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import qualified Data.Text as T

-- | Config path for the tracking ID
trackingIdPath :: Text
trackingIdPath = "frontend/google/analytics/tracking-id"

type TrackingId = Text

-- | Embed Google analytics scripts. This function will throw if the tracking ID
-- config is missing.
googleAnalyticsFromConfig :: (HasConfigs m, Prerender js t m, PerformEvent t m, DomBuilder t m, Routed t r m) => m ()
googleAnalyticsFromConfig = getTrackingId >>= \case
   Nothing -> fail $ T.unpack $ T.unwords
     [ "Google analytics tracking ID not found in configs."
     , "Expected path: " <> trackingIdPath
     ]
   Just tid -> googleAnalytics tid

-- | Embed Google analytics scripts with the given tracking ID.
googleAnalytics :: (DomBuilder t m, Prerender js t m, PerformEvent t m, Routed t r m) => TrackingId -> m ()
googleAnalytics trackingId = do
  let gtagSrc = "https://www.googletagmanager.com/gtag/js?id=" <> trackingId
      escapedTrackingId = T.pack $ show trackingId
  elAttr "script" ("async" =: "async" <> "src" =: gtagSrc) blank
  el "script" $ text $ T.unlines
    [ "window.dataLayer = window.dataLayer || [];"
    , "function gtag(){dataLayer.push(arguments);}"
    , "gtag('js', new Date());"
    , "gtag('config', " <> escapedTrackingId <> ");"
    ]
  route <- askRoute
  prerender_ blank $ do
    performEvent_ $ ffor (updated route) $ \_ -> liftJSM $ do
      void $ eval $ "gtag('config', " <> escapedTrackingId <> ", { 'page_path': location.pathname, 'page_title': document.title });"

-- | Get the tracking ID from the configuration. The config file should be
-- located at 'trackingIdPath'.
getTrackingId :: HasConfigs m => m (Maybe TrackingId)
getTrackingId = fmap T.strip <$> getTextConfig trackingIdPath
