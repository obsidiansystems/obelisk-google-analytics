{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Obelisk.Frontend.GoogleAnalytics
  ( requireGoogleAnalytics
  , googleAnalytics
  , getTrackingId
  , trackingIdPath
  ) where

import Data.Text (Text)
import Obelisk.Configs
import Reflex.Dom.Core
import qualified Data.Text as T

-- | Config path for the tracking ID
trackingIdPath :: Text
trackingIdPath = "frontend/google/analytics/tracking-id"

type TrackingId = Text

-- | Embed Google analytics scripts. This function will throw if the tracking ID
-- config is missing.
requireGoogleAnalytics :: (HasConfigs m, DomBuilder t m) => m ()
requireGoogleAnalytics = getTrackingId >>= \case
  Nothing -> fail $ T.unpack $ T.unwords
    [ "Google analytics tracking ID not found in configs."
    , "Expected path: " <> trackingIdPath
    ]
  Just tid -> googleAnalytics tid

-- | Embed Google analytics scripts with the given tracking ID.
googleAnalytics :: DomBuilder t m => TrackingId -> m ()
googleAnalytics trackingId = do
  let gtagSrc = "https://www.googletagmanager.com/gtag/js?id=" <> trackingId
  elAttr "script" ("async" =: "" <> "src" =: gtagSrc) blank
  el "script" $ text $ T.unlines
    [ "window.dataLayer = window.dataLayer || [];"
    , "function gtag(){dataLayer.push(arguments);}"
    , "gtag('js', new Date());"
    , "gtag('config', '" <> trackingId <> "');"
    ]

-- | Get the tracking ID from the configuration. The config file should be
-- located at 'trackingIdPath'.
getTrackingId :: HasConfigs m => m (Maybe TrackingId)
getTrackingId = fmap T.strip <$> getTextConfig trackingIdPath

