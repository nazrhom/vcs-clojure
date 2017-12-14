(ns ring.middleware.file
  "Static file serving."
  (:import java.io.File)
  (:require [ring.util.codec :as codec]
            [ring.util.response :as response]
            [ring.util.request :as request]
            [ring.middleware.head :as head]
            [clojure.java.io :as io]))

(defn wrap-file
  "Wrap an handler such that the directory at the given root-path is checked for
  a static file with which to respond to the request, proxying the request to
  the wrapped handler if such a file does not exist.

  An map of options may be optionally specified. These options will be passed
  to the ring.util.response/file-response function."
  [handler ^String root-path & [opts]]
  (ensure-dir root-path)
  (fn [req]
    (or ((head/wrap-head #(file-request % root-path opts)) req)
        (handler req))))
