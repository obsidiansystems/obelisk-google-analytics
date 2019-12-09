{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Obelisk.Frontend.GoogleAnalytics
  ( googleAnalyticsFromConfig
  , googleAnalytics
  , trackingIdPath
  , getTrackingId
  , Analytics(..)
  , GoogleAnalyticsT(..)
  , GtagJSCall(..)
  , runGoogleAnalyticsT
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Text (Text)
import GHCJS.DOM.Types (liftJSM)
import Language.Javascript.JSaddle.Evaluate (eval)
import Obelisk.Configs
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Language.Javascript.JSaddle.Types

class Monad m => Analytics t w m | m -> w where
  tellAnalytics :: Event t w -> m ()
  -- ^ Tell analytics something, with asynchronous delivery.

  -- tellAnalyticsSync :: w -> m ()
  -- ^ Tell analytics something, with synchronous delivery.
  --   This waits until the message is delivered before returning.

  -- tellAnalyticsWithSync :: w -> m (STM Bool)
  -- ^ Tell analytics something, with asynchronous delivery.
  --   Returns a 'STM' action that becomes 'True' after delivery of this particular message.
  --   This is the preferred way to implement synchronous delivery with timeout.

  -- syncAnalytics :: m ()
  -- ^ Synchronize on the delivery of every message outstanding at the time of the call.
  --   This returns after all such messages have been delivered.

  -- asyncAnalytics :: m (STM Bool)
  -- ^ Returns an 'STM' action that becomes 'True' after all currently outstanding messages have been delivered.
  --   This is an asynchronous variant of 'syncAnalytics', and the preferred way of doing a "global" sync with timeout.

data GtagJSCall = GtagJSCall
  { _GtagJSCall_Event_method :: !Text
  , _GtagJSCall_Event_action :: !Text
  , _GtagJSCall_Event_params :: !Aeson.Value
  }

newtype GoogleAnalyticsT t w m a = GoogleAnalyticsT
  { unGoogleAnalyticsT :: EventWriterT t (Seq w) m a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)  -- , MonadException, MonadAsyncException)

instance MonadTrans (GoogleAnalyticsT t w) where
  lift = GoogleAnalyticsT . lift

runGoogleAnalyticsT
  :: ( DomBuilder t m
     , Prerender js t m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , Routed t r m
     )
  => (w -> GtagJSCall)
  -> GoogleAnalyticsT t w m a
  -> m a

runGoogleAnalyticsT interpW (GoogleAnalyticsT m) = do
  (a, evt) <- runEventWriterT m
  performEvent_ ( gtag <$> evt )
  return a
  where
    gtag ws = do
      forM_ ws $ \w -> do
        let call = interpW w
            escape x = T.pack $ show $ x
            escapedMethod = escape $ _GtagJSCall_Event_method call
            escapedAction = escape $ _GtagJSCall_Event_action call
            escapedParams = escape $ Aeson.encode $ _GtagJSCall_Event_method call
        liftJSM $ void $ eval $ "gtag(" <> escapedMethod <> ", " <> escapedAction <> ", " <> escapedParams <> ");"

instance (Reflex t, Monad m) => Analytics t w (GoogleAnalyticsT t w m) where
  tellAnalytics =  GoogleAnalyticsT . tellEvent . (Seq.singleton <$>)

type TrackingId = Text

-- | Config path for the tracking ID
trackingIdPath :: Text
trackingIdPath = "frontend/google/analytics/tracking-id"

-- | Embed Google analytics scripts. This function will throw if the tracking ID
-- config is missing.
googleAnalyticsFromConfig :: (HasConfigs m, Prerender js t m, PerformEvent t m, DomBuilder t m, Routed t r m) => m ()
googleAnalyticsFromConfig = getTrackingId >>= googleAnalytics

-- | Embed Google analytics scripts with the given tracking ID.
googleAnalytics :: (DomBuilder t m, Prerender js t m, PerformEvent t m, Routed t r m) => Maybe TrackingId -> m ()
googleAnalytics mTrackingId = do
  case mTrackingId of
    Nothing -> do
      el "script" $ text "function gtag(){return;}"
    Just trackingId -> do
      let gtagSrc = "https://www.googletagmanager.com/gtag/js?id=" <> trackingId
          escapedTrackingId = T.pack $ show trackingId   -- TODO: is this realy the correct way to escape?
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
