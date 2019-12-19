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

Simply include the `googleAnalytics` widget at the top of the page head; if you want to add additional events on the body,  then put `mapRoutedT runGoogleAnalyticsT` around your `_frontend_body`

```
frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = googleAnalytics >> your_frontend_head
  , _frontend_body = mapRoutedT runGoogleAnalyticsT $ your_frontend_body
  }

```
