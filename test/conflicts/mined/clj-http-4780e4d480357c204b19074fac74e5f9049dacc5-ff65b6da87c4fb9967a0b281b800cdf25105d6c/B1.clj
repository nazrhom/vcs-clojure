(ns clj-http.core
  "Core HTTP request/response implementation."
  (:require [clojure.pprint])
  (:import (java.io File InputStream)
           (java.net URI)
           (org.apache.http HttpRequest HttpEntityEnclosingRequest
                            HttpResponse Header HttpHost)
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

(defn request
  "Executes the HTTP request corresponding to the given Ring request map and
   returns the Ring response map corresponding to the resulting HTTP response.

   Note that where Ring uses InputStreams for the request and response bodies,
   the clj-http uses ByteArrays for the bodies."
  [{:keys [request-method scheme server-name server-port uri query-string
           headers content-type character-encoding body socket-timeout
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
