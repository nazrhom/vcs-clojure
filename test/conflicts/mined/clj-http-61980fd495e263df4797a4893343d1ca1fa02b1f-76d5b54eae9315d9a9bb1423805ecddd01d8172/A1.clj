(defn- coerce-body-entity
  "Coerce the http-entity from an HttpResponse to a stream that closes itself
  and the connection manager when closed."
  [^HttpEntity http-entity ^HttpClientConnectionManager conn-mgr ^CloseableHttpResponse response]
  (when http-entity
    (proxy [FilterInputStream]
        [^InputStream (.getContent http-entity)]
      (close []
        (try
          ;; Eliminate the reflection warning from proxy-super
          (let [^InputStream this this]
            (proxy-super close))
          (finally
            (.close response)
            (when-not (conn/reusable? conn-mgr)
              (.shutdown conn-mgr))))))
    (when-not (conn/reusable? conn-mgr)
      (.shutdown conn-mgr))))

(defn request
  [{:keys [body
           conn-timeout
           connection-manager
           cookie-store
           headers
           multipart
           query-string
           redirect-strategy
           retry-handler
           request-method
           scheme
           server-name
           server-port
           socket-timeout
           uri]
    :as req}]
  (let [scheme (name scheme)
        http-url (str scheme "://" server-name
                      (when server-port (str ":" server-port))
                      uri
                      (when query-string (str "?" query-string)))
        conn-mgr (or connection-manager (conn/basic-conn-mgr))
        ^RequestConfig request-config (-> (RequestConfig/custom)
                                          (.setConnectTimeout (or conn-timeout -1))
                                          (.setSocketTimeout (or socket-timeout -1))
                                          (.build))
        ^CloseableHttpClient client (http-client conn-mgr
                                                 redirect-strategy
                                                 retry-handler)
        ^HttpClientContext context (http-context request-config)
        ^HttpUriRequest http-req (http-request-for request-method http-url body)]
    (when-not (conn/reusable? conn-mgr)
      (.addHeader http-req "Connection" "close"))
    (when cookie-store
      (.setCookieStore context cookie-store))
    (if multipart
      (.setEntity ^HttpEntityEnclosingRequest http-req
                  (mp/create-multipart-entity multipart))
      (when (and body (instance? HttpEntityEnclosingRequest http-req))
        (if (instance? HttpEntity body)
          (.setEntity ^HttpEntityEnclosingRequest http-req body)
          (.setEntity ^HttpEntityEnclosingRequest http-req
                      (if (string? body)
                        (StringEntity. ^String body "UTF-8")
                        (ByteArrayEntity. body))))))
    (doseq [[header-n header-v] headers]
      (if (coll? header-v)
        (doseq [header-vth header-v]
          (.addHeader http-req header-n header-vth))
        (.addHeader http-req header-n (str header-v))))
    (when (opt req :debug) (print-debug! req http-req))
    (let [^CloseableHttpResponse response (.execute client http-req context)
          ^HttpEntity entity (.getEntity response)
          status (.getStatusLine response)]
      {:body (coerce-body-entity entity conn-mgr response)
       :headers (parse-headers
                 (.headerIterator response)
                 (opt req :use-header-maps-in-response))
       :length (if (nil? entity) 0 (.getContentLength entity))
       :chunked? (if (nil? entity) false (.isChunked entity))
       :repeatable? (if (nil? entity) false (.isRepeatable entity))
       :streaming? (if (nil? entity) false (.isStreaming entity))
       :status (.getStatusCode status)})))
