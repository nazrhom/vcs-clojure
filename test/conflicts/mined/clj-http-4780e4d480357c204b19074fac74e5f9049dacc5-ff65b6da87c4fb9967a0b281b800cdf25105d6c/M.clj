(ns clj-http.core
  "Core HTTP request/response implementation."
  (:require [clojure.pprint])
  (:import (java.io File InputStream)
           (java.net URI)
           (org.apache.http HttpRequest HttpEntityEnclosingRequest
                            HttpResponse Header HttpHost)
<<<<<<< A.clj
           (org.apache.http.util EntityUtils)
           (org.apache.http.entity ByteArrayEntity)
           (org.apache.http.entity.mime MultipartEntity)
           (org.apache.http.entity.mime.content ByteArrayBody
                                                FileBody
                                                InputStreamBody
                                                StringBody)
||||||| O.clj
           (org.apache.http.util EntityUtils)
           (org.apache.http.entity ByteArrayEntity)
=======
>>>>>>> B.clj
           (org.apache.http.client HttpClient)
           (org.apache.http.client.methods HttpGet HttpHead HttpPut
                                           HttpPost HttpDelete
                                           HttpEntityEnclosingRequestBase)
           (org.apache.http.client.params CookiePolicy ClientPNames)
           (org.apache.http.conn.params ConnRoutePNames)
           (org.apache.http.conn.scheme PlainSocketFactory
                                        SchemeRegistry Scheme)
           (org.apache.http.conn.ssl SSLSocketFactory TrustStrategy)
           (org.apache.http.entity ByteArrayEntity)
           (org.apache.http.impl.conn SingleClientConnManager)
           (org.apache.http.impl.conn.tsccm ThreadSafeClientConnManager)
           (org.apache.http.impl.client DefaultHttpClient)
           (org.apache.http.util EntityUtils)))

(defn- parse-headers [#^HttpResponse http-resp]
  (into {} (map (fn [#^Header h] [(.toLowerCase (.getName h)) (.getValue h)])
                (iterator-seq (.headerIterator http-resp)))))

(defn- set-client-param [#^HttpClient client key val]
  (when (not (nil? val))
    (-> client
        (.getParams)
        (.setParameter key val))))

(defn- proxy-delete-with-body [url]
  (let [res (proxy [HttpEntityEnclosingRequestBase] []
              (getMethod [] "DELETE"))]
    (.setURI res (URI. url))
    res))

(def ^{:private true} insecure-socket-factory
  (doto (SSLSocketFactory. (reify TrustStrategy
                             (isTrusted [_ _ _] true)))
    (.setHostnameVerifier SSLSocketFactory/ALLOW_ALL_HOSTNAME_VERIFIER)))

(def ^{:private true} insecure-scheme-registry
  (doto (SchemeRegistry.)
    (.register (Scheme. "http" 80 (PlainSocketFactory/getSocketFactory)))
    (.register (Scheme. "https" 443 insecure-socket-factory))))

(def ^{:private true} regular-scheme-registry
  (doto (SchemeRegistry.)
    (.register (Scheme. "http" 80 (PlainSocketFactory/getSocketFactory)))
    (.register (Scheme. "https" 443 (SSLSocketFactory/getSocketFactory)))))

(defn- make-regular-conn-manager [& [insecure?]]
  (if insecure?
    (SingleClientConnManager. insecure-scheme-registry)
    (SingleClientConnManager.)))

<<<<<<< A.clj
(defn- create-multipart-entity
  "Takes a multipart map and creates a MultipartEntity with each key/val pair
   added as a part, determining part type by the val type."
  [multipart]
  (let [mp-entity (MultipartEntity.)]
    (doseq [[k v] multipart]
      (let [klass (type v)
            filename (name k)
            part (cond
                  (= klass File)
                  (FileBody. v filename)

                  (= klass InputStream)
                  (InputStreamBody. v filename)

                  (= klass (type (byte-array 0)))
                  (ByteArrayBody. v filename)

                  (= klass String)
                  (StringBody. v))]
        (.addPart mp-entity filename part)))
    mp-entity))

||||||| O.clj
=======
(defn make-reusable-conn-manager
  "Given an timeout and optional insecure? flag, create a
  ThreadSafeClientConnManager with <timeout> seconds set as the timeout value."
  [timeout & [insecure?]]
  (let [sr (if insecure? insecure-scheme-registry regular-scheme-registry)]
    (ThreadSafeClientConnManager.
     sr timeout java.util.concurrent.TimeUnit/SECONDS)))

(def ^{:dynamic true} *connection-manager* nil)

>>>>>>> B.clj
(defn request
  "Executes the HTTP request corresponding to the given Ring request map and
   returns the Ring response map corresponding to the resulting HTTP response.

   Note that where Ring uses InputStreams for the request and response bodies,
   the clj-http uses ByteArrays for the bodies."
  [{:keys [request-method scheme server-name server-port uri query-string
           headers content-type character-encoding body socket-timeout
<<<<<<< A.clj
           conn-timeout multipart debug insecure?] :as req}]
  (let [http-client (DefaultHttpClient. (connection-manager insecure?))]
    (try
      (doto http-client
        (set-client-param ClientPNames/COOKIE_POLICY
                          CookiePolicy/BROWSER_COMPATIBILITY)
        (set-client-param ClientPNames/HANDLE_REDIRECTS false)
        (set-client-param "http.socket.timeout"
                          (and socket-timeout (Integer. socket-timeout)))
        (set-client-param "http.connection.timeout"
                          (and conn-timeout (Integer. conn-timeout))))
      (when-not (#{"localhost" "127.0.0.1"} server-name)
        (when-let [proxy-host (System/getProperty (str scheme ".proxyHost"))]
          (let [proxy-port (Integer/parseInt
                            (System/getProperty (str scheme ".proxyPort")))]
            (set-client-param http-client ConnRoutePNames/DEFAULT_PROXY
                              (HttpHost. proxy-host proxy-port)))))
      (let [http-url (str scheme "://" server-name
                          (when server-port (str ":" server-port))
                          uri
                          (when query-string (str "?" query-string)))
            #^HttpRequest
            http-req (case request-method
                       :get    (HttpGet. http-url)
                       :head   (HttpHead. http-url)
                       :put    (HttpPut. http-url)
                       :post   (HttpPost. http-url)
                       :delete (proxy-delete-with-body http-url))]
        (when (and content-type character-encoding)
          (.addHeader http-req "Content-Type"
                      (str content-type "; charset=" character-encoding)))
        (when (and content-type (not character-encoding))
          (.addHeader http-req "Content-Type" content-type))
        (.addHeader http-req "Connection" "close")
        (doseq [[header-n header-v] headers]
          (.addHeader http-req header-n header-v))
        (if multipart
          (let [mp-entity (MultipartEntity.)]
            (doseq [[k v] multipart]
              (.addPart mp-entity (name k) (ByteArrayBody. v (name k))))
            (.setEntity #^HttpEntityEnclosingRequest http-req
                        (create-multipart-entity multipart)))
          (when body
            (let [http-body (ByteArrayEntity. body)]
              (.setEntity #^HttpEntityEnclosingRequest http-req http-body))))
        (when debug
          (println "Request:")
          (clojure.pprint/pprint req)
          (println "HttpRequest:")
          (clojure.pprint/pprint (bean http-req)))
        (let [http-resp (.execute http-client http-req)
              http-entity (.getEntity http-resp)
              resp {:status (.getStatusCode (.getStatusLine http-resp))
                    :headers (parse-headers http-resp)
                    :body (when http-entity
                            (EntityUtils/toByteArray http-entity))}]
          (.shutdown (.getConnectionManager http-client))
          resp)))))
||||||| O.clj
           conn-timeout debug insecure?] :as req}]
  (let [http-client (DefaultHttpClient. (connection-manager insecure?))]
    (try
      (doto http-client
        (set-client-param ClientPNames/COOKIE_POLICY
                          CookiePolicy/BROWSER_COMPATIBILITY)
        (set-client-param ClientPNames/HANDLE_REDIRECTS false)
        (set-client-param "http.socket.timeout"
                          (and socket-timeout (Integer. socket-timeout)))
        (set-client-param "http.connection.timeout"
                          (and conn-timeout (Integer. conn-timeout))))
      (when (nil? (#{"localhost" "127.0.0.1"} server-name))
        (when-let [proxy-host (System/getProperty (str scheme ".proxyHost"))]
          (let [proxy-port (Integer/parseInt
                            (System/getProperty (str scheme ".proxyPort")))]
            (set-client-param http-client ConnRoutePNames/DEFAULT_PROXY
                              (HttpHost. proxy-host proxy-port)))))
      (let [http-url (str scheme "://" server-name
                          (when server-port (str ":" server-port))
                          uri
                          (when query-string (str "?" query-string)))
            #^HttpRequest
            http-req (case request-method
                       :get    (HttpGet. http-url)
                       :head   (HttpHead. http-url)
                       :put    (HttpPut. http-url)
                       :post   (HttpPost. http-url)
                       :delete (proxy-delete-with-body http-url))]
        (when (and content-type character-encoding)
          (.addHeader http-req "Content-Type"
                      (str content-type "; charset=" character-encoding)))
        (when (and content-type (not character-encoding))
          (.addHeader http-req "Content-Type" content-type))
        (.addHeader http-req "Connection" "close")
        (doseq [[header-n header-v] headers]
          (.addHeader http-req header-n header-v))
        (when body
          (let [http-body (ByteArrayEntity. body)]
            (.setEntity #^HttpEntityEnclosingRequest http-req http-body)))
        (when debug
          (println "Request:")
          (clojure.pprint/pprint req)
          (println "HttpRequest:")
          (clojure.pprint/pprint (bean http-req)))
        (let [http-resp (.execute http-client http-req)
              http-entity (.getEntity http-resp)
              resp {:status (.getStatusCode (.getStatusLine http-resp))
                    :headers (parse-headers http-resp)
                    :body (when http-entity
                            (EntityUtils/toByteArray http-entity))}]
          (.shutdown (.getConnectionManager http-client))
          resp)))))
=======
           conn-timeout debug insecure?] :as req}]
  (let [conn-mgr (or *connection-manager*
                     (make-regular-conn-manager insecure?))
        http-client (DefaultHttpClient. conn-mgr)]
    (doto http-client
      (set-client-param ClientPNames/COOKIE_POLICY
                        CookiePolicy/BROWSER_COMPATIBILITY)
      (set-client-param ClientPNames/HANDLE_REDIRECTS false)
      (set-client-param "http.socket.timeout"
                        (and socket-timeout (Integer. socket-timeout)))
      (set-client-param "http.connection.timeout"
                        (and conn-timeout (Integer. conn-timeout))))
    (when (nil? (#{"localhost" "127.0.0.1"} server-name))
      (when-let [proxy-host (System/getProperty (str scheme ".proxyHost"))]
        (let [proxy-port (Integer/parseInt
                          (System/getProperty (str scheme ".proxyPort")))]
          (set-client-param http-client ConnRoutePNames/DEFAULT_PROXY
                            (HttpHost. proxy-host proxy-port)))))
    (let [http-url (str scheme "://" server-name
                        (when server-port (str ":" server-port))
                        uri
                        (when query-string (str "?" query-string)))
          #^HttpRequest
          http-req (case request-method
                     :get    (HttpGet. http-url)
                     :head   (HttpHead. http-url)
                     :put    (HttpPut. http-url)
                     :post   (HttpPost. http-url)
                     :delete (proxy-delete-with-body http-url))]
      (when (and content-type character-encoding)
        (.addHeader http-req "Content-Type"
                    (str content-type "; charset=" character-encoding)))
      (when (and content-type (not character-encoding))
        (.addHeader http-req "Content-Type" content-type))
      (when (instance? SingleClientConnManager conn-mgr)
        (.addHeader http-req "Connection" "close"))
      (doseq [[header-n header-v] headers]
        (.addHeader http-req header-n header-v))
      (when body
        (let [http-body (ByteArrayEntity. body)]
          (.setEntity #^HttpEntityEnclosingRequest http-req http-body)))
      (when debug
        (println "Request:")
        (clojure.pprint/pprint (assoc req :body (format "... %s bytes ..."
                                                        (count (:body req)))))
        (println "HttpRequest:")
        (clojure.pprint/pprint (bean http-req)))
      (let [http-resp (.execute http-client http-req)
            http-entity (.getEntity http-resp)
            resp {:status (.getStatusCode (.getStatusLine http-resp))
                  :headers (parse-headers http-resp)
                  :body (when http-entity
                          (EntityUtils/toByteArray http-entity))}]
        (when (instance? SingleClientConnManager conn-mgr)
          (.shutdown (.getConnectionManager http-client)))
        resp))))
>>>>>>> B.clj
