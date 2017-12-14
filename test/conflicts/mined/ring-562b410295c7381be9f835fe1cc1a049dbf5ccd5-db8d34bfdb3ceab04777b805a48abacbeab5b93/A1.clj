(ns ring.middleware.file
  "Middleware to serve files from a directory.

  Most of the time you should prefer ring.middleware.resource instead, as this
  middleware will not work with files in jar or war files."
  (:require [ring.util.codec :as codec]
            [ring.util.response :as response]
            [ring.util.request :as request]
            [ring.middleware.head :as head])
  (:import [java.io File]))

(defn wrap-file
  "Wrap an handler such that the directory at the given root-path is checked for
  a static file with which to respond to the request, proxying the request to
  the wrapped handler if such a file does not exist.

  Accepts the following options:

  :index-files?    - look for index.* files in directories, defaults to true
  :allow-symlinks? - serve files through symbolic links, defaults to false"
  {:arglists '([handler root-path] [handler root-path options])}
  [handler ^String root-path & [opts]]
  (ensure-dir root-path)
  (fn [req]
    (or ((head/wrap-head #(file-request % root-path opts)) req)
        (handler req))))
