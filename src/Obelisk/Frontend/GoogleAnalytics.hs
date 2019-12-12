{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Frontend.GoogleAnalytics
  ( googleAnalyticsFromConfig
  , googleAnalytics
  , trackingIdPath
  , getTrackingId
  , Analytics(..)
  , GoogleAnalyticsT(..)
  , GtagJSCall(..)
  , gaClickEvent
  , gaOutboundClickEvent
  , runGoogleAnalyticsT
  ) where

import Control.Monad
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Text (Text)
import Language.Javascript.JSaddle.Evaluate (eval)
import Obelisk.Configs
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Language.Javascript.JSaddle
import Reflex.Host.Class

import qualified Data.HashMap.Strict as HMap

class Monad m => Analytics t w m | m -> w where
  tellAnalytics :: Event t w -> m ()
  -- ^ Tell analytics something, with asynchronous delivery.

  -- tellAnalyticsSync :: Event t w -> m ()
  -- ^ Tell analytics something, with synchronous delivery.
  --   This waits until the message is delivered before returning.

  -- tellAnalyticsWithSync :: Event t w -> m (STM Bool)    <- this type is conceptually flawed
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
  { _gtagJSCall_method :: !Text
  , _gtagJSCall_action :: !Text
  , _gtagJSCall_params :: !Aeson.Value
  }

gaOutboundClickEvent :: Text -> GtagJSCall
gaOutboundClickEvent = gaClickEvent "outbound"

gaClickEvent :: Text -> Text -> GtagJSCall
gaClickEvent category label = GtagJSCall
  { _gtagJSCall_method = "event"
  , _gtagJSCall_action = "click"
  , _gtagJSCall_params =
      Aeson.Object
        (HMap.fromList
          [ ("event_category", Aeson.String category)
          , ("event_label", Aeson.String label)
          ]
        )
  }

newtype GoogleAnalyticsT t w m a = GoogleAnalyticsT
  { unGoogleAnalyticsT :: EventWriterT t (Seq w) m a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance (Monad m, RouteToUrl r m) => RouteToUrl r (GoogleAnalyticsT t w m)

instance NotReady t m => NotReady t (GoogleAnalyticsT t w m)

instance (Reflex t, Adjustable t m, MonadHold t m) => Adjustable t (GoogleAnalyticsT t w m) where
  runWithReplace (GoogleAnalyticsT m) e =
    GoogleAnalyticsT (runWithReplace m (unGoogleAnalyticsT <$> e))
  traverseIntMapWithKeyWithAdjust f dm0 dm' =
    GoogleAnalyticsT (traverseIntMapWithKeyWithAdjust (\x y -> unGoogleAnalyticsT (f x y)) dm0 dm')
  traverseDMapWithKeyWithAdjust f dm0 dm' =
    GoogleAnalyticsT (traverseDMapWithKeyWithAdjust (\x y -> unGoogleAnalyticsT (f x y)) dm0 dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' =
    GoogleAnalyticsT (traverseDMapWithKeyWithAdjustWithMove (\x y -> unGoogleAnalyticsT (f x y)) dm0 dm')

instance (Monad m, Routed t r m) => Routed t r (GoogleAnalyticsT t w m)

instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (GoogleAnalyticsT t w m) where
  type DomBuilderSpace (GoogleAnalyticsT t w m) = DomBuilderSpace m
  textNode = liftTextNode
  commentNode = liftCommentNode
  element elementTag cfg (GoogleAnalyticsT child) = GoogleAnalyticsT $ element elementTag cfg child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (GoogleAnalyticsT child) = GoogleAnalyticsT $ selectElement cfg child
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

instance MonadTrans (GoogleAnalyticsT t w) where
  lift = GoogleAnalyticsT . lift

instance (Reflex t, Monad m, SetRoute t r m) => SetRoute t r (GoogleAnalyticsT t w m)

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (GoogleAnalyticsT t w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance PrimMonad m => PrimMonad (GoogleAnalyticsT t w m) where
  type PrimState (GoogleAnalyticsT t w m) = PrimState m
  primitive = lift . primitive

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (GoogleAnalyticsT t r m)
#endif

runGoogleAnalyticsT
  :: ( DomBuilder t m
     , Prerender js t m
     )
  => (w -> GtagJSCall)
  -> GoogleAnalyticsT t w m a
  -> m a
runGoogleAnalyticsT interpW (GoogleAnalyticsT m) = do
  (a, evt) <- runEventWriterT m
  prerender_ blank $ performEvent_ ( fmap gtag evt )
  return a
  where
    gtag ws = do
      liftJSM $ forM_ ws $ \w -> do
        let c = interpW w
        void $ jsg3 ("gtag"::Text) (_gtagJSCall_method c) (_gtagJSCall_action c) (_gtagJSCall_params c)

instance (Reflex t, Monad m) => Analytics t w (GoogleAnalyticsT t w m) where
  tellAnalytics = GoogleAnalyticsT . tellEvent . (Seq.singleton <$>)

instance (Reflex t, Monad m, Analytics t w m) => Analytics t w (RoutedT t r m) where
  tellAnalytics = lift . tellAnalytics

instance (Reflex t, Monad m, PostBuild t m) => PostBuild t (GoogleAnalyticsT t w m) where
  getPostBuild = GoogleAnalyticsT getPostBuild

instance (MonadJSM (GoogleAnalyticsT t w (Client m)), Prerender js t m, Monad m, Reflex t) => Prerender js t (GoogleAnalyticsT t w m) where
  type Client (GoogleAnalyticsT t w m) = GoogleAnalyticsT t w (Client m)
  prerender server client = do
    GoogleAnalyticsT $ prerender (unGoogleAnalyticsT server) (unGoogleAnalyticsT client)

instance MonadRef m => MonadRef (GoogleAnalyticsT t w m) where
  type Ref (GoogleAnalyticsT t w m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance PerformEvent t m => PerformEvent t (GoogleAnalyticsT t w m) where
  type Performable (GoogleAnalyticsT t w m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

deriving instance DomRenderHook t m => DomRenderHook t (GoogleAnalyticsT t w m)
instance HasDocument m => HasDocument (GoogleAnalyticsT t w m)

instance TriggerEvent t m => TriggerEvent t (GoogleAnalyticsT t w m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance HasJSContext m => HasJSContext (GoogleAnalyticsT t w m) where
  type JSContextPhantom (GoogleAnalyticsT t w m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance MonadSample t m => MonadSample t (GoogleAnalyticsT t w m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (GoogleAnalyticsT t w m) where
  {-# INLINABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE

instance HasJS x m => HasJS x (GoogleAnalyticsT t w m) where
  type JSX (GoogleAnalyticsT t w m) = JSX m
  liftJS = lift . liftJS

type TrackingId = Text

-- | Config path for the tracking ID
trackingIdPath :: Text
trackingIdPath = "frontend/google/analytics/tracking-id"

-- | Embed Google analytics scripts. This function will throw if the tracking ID
-- config is missing.
googleAnalyticsFromConfig :: (HasConfigs m, Prerender js t m, DomBuilder t m, Routed t r m) => m ()
googleAnalyticsFromConfig = getTrackingId >>= googleAnalytics

-- | Embed Google analytics scripts with the given tracking ID.
googleAnalytics :: (DomBuilder t m, Prerender js t m, Routed t r m) => Maybe TrackingId -> m ()
googleAnalytics mTrackingId =
  case mTrackingId of
    Nothing ->
      el "script" $ text "function gtag(){return;}"
    Just trackingId -> do
      -- TODO:  use proper escaping for tracking ID on url and fix the gtag('config' ... ) call
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
      prerender_ blank $
        performEvent_ $ ffor (updated route) $ \_ -> liftJSM $ do
          (Just pathname) <- fromJSVal =<< eval ("location.pathname"::Text)
          (Just docTitle) <- fromJSVal =<< eval ("document.title"::Text)
          void $ jsg3 ("gtag"::Text) ("config"::Text) trackingId $ toJSVal (Aeson.Object (HMap.fromList [("page_path", Aeson.String pathname), ("page_title", Aeson.String docTitle)]))

-- | Get the tracking ID from the configuration. The config file should be
-- located at 'trackingIdPath'.
getTrackingId :: HasConfigs m => m (Maybe TrackingId)
getTrackingId = fmap T.strip <$> getTextConfig trackingIdPath
