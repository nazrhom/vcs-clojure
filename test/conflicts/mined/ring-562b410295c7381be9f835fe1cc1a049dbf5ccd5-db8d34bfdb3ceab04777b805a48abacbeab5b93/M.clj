(ns ring.middleware.file
<<<<<<< A.clj
  "Middleware to serve files from a directory.

  Most of the time you should prefer ring.middleware.resource instead, as this
  middleware will not work with files in jar or war files."
||||||| O.clj
  "Static file serving."
  (:import java.io.File)
=======
  "Static file serving."
>>>>>>> B.clj
  (:require [ring.util.codec :as codec]
            [ring.util.response :as response]
            [ring.util.request :as request]
<<<<<<< A.clj
            [ring.middleware.head :as head])
  (:import [java.io File]))
||||||| O.clj
            [ring.middleware.head :as head]))
=======
            [ring.middleware.head :as head]
            [clojure.java.io :as io]))
>>>>>>> B.clj

(defn- ensure-dir
  "Ensures that a directory exists at the given path, throwing if one does not."
  [dir-path]
  (let [dir (io/as-file dir-path)]
    (if-not (.exists dir)
      (throw (Exception. (format "Directory does not exist: %s" dir-path))))))

(defn file-request
  "If request matches a static file, returns it in a response. Otherwise
  returns nil. See: wrap-file."
  {:arglists '([request root-path] [request root-path options])
   :added "1.2"}
  [req root-path & [opts]]
  (let [opts (merge {:root (str root-path), :index-files? true, :allow-symlinks? false} opts)]
    (if (= :get (:request-method req))
      (let [path (subs (codec/url-decode (request/path-info req)) 1)]
        (response/file-response path opts)))))

(defn wrap-file
  "Wrap an handler such that the directory at the given root-path is checked for
  a static file with which to respond to the request, proxying the request to
  the wrapped handler if such a file does not exist.

<<<<<<< A.clj
  Accepts the following options:

  :index-files?    - look for index.* files in directories, defaults to true
  :allow-symlinks? - serve files through symbolic links, defaults to false"
  {:arglists '([handler root-path] [handler root-path options])}
  [handler ^String root-path & [opts]]
||||||| O.clj
  An map of options may be optionally specified. These options will be passed
  to the ring.util.response/file-response function."
  [handler ^String root-path & [opts]]
=======
  An map of options may be optionally specified. These options will be passed
  to the ring.util.response/file-response function."
  [handler root-path & [opts]]
>>>>>>> B.clj
  (ensure-dir root-path)
  (fn [req]
    (or ((head/wrap-head #(file-request % root-path opts)) req)
        (handler req))))
