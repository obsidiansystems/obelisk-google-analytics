# Obelisk Google Analytics

Add google analytics tracking to your obelisk apps. Automatically sends `pageview` information when routes change.  Also allows you to add more find-grained events to a reflex application.

This library is a thin binding to `gtag.js`,  one of several offically supported client libraries for Google Analytics.

## Configuration

Go to https://analytics.google.com/analytics/web/, and set up an account if needed.  From there, you can find a copy-and-pastable tracking ID you want to use for a given deployment, by clicking on the "Admin" settings in the lower right of the page,  then selecting a Property at the top of the middle column,  and then clicking "Property Settings".  This tracking ID needs be put into a file located at:

```
config/frontend/google/analytics/tracking-id
```

Note that if this file doesn't exist,  analytics will be disabled, and no events will be sent to Google.

## Use in a Reflex Project

### Importing analytics

Simply include the `googleAnalytics` widget at the beginning of your `_frontend_head`:  this will automatically capture the current route being viewed.

```
frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = googleAnalytics >> your_frontend_head
  , _frontend_body = your_frontend_body
  }
```

### Adding additional events

In addition the previous section, also put `mapRoutedT runGoogleAnalyticsT` around your `_frontend_body`.  This will allow you to use `tellAnalytics` anywhere on the site.

```
frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = googleAnalytics >> your_frontend_head
  , _frontend_body = mapRoutedT runGoogleAnalyticsT $ your_frontend_body
  }
```

We use `mapRoutedT` in order to put the `GoogleAnalyticsT` underneath the `RouteT`,  and avoid having to generalize routing functions such as `subRoute_` in tricky ways.  Here's an example of a widget that opens a external link in new window,  and sends an analytics event when it happens:

```
extLinkAttr :: forall t m a. (DomBuilder t m, Analytics t m) => Map Text Text -> Text -> m a -> m a
extLinkAttr attrs href m = do
  (e,a) <- elAttr' "a" ("href" =: href <> attrs <> "target" =: "_blank" <> "rel" =: "noopener") m
  tellAnalytics (gaOutboundClickEvent href <$ (domEvent Click e :: Event t ()))
  return a
```
