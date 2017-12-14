(ns clj-http.cookies
  (:require [clj-http.util :refer [opt]]
            [clojure.string :refer [blank? join lower-case]])
  (:import (org.apache.http.cookie ClientCookie CookieOrigin CookieSpec)
           (org.apache.http.impl.cookie BasicClientCookie2)
           (org.apache.http.impl.cookie BrowserCompatSpecFactory)
           (org.apache.http.message BasicHeader)
           org.apache.http.client.CookieStore
           (org.apache.http.impl.client BasicCookieStore)
           (org.apache.http Header)
           (org.apache.http.protocol BasicHttpContext)))

(defn cookie-spec ^CookieSpec []
  (.create
   (BrowserCompatSpecFactory.)
   (BasicHttpContext.)))