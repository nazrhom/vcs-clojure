(deftest servlet-test
  (let [body (proxy [javax.servlet.ServletInputStream] [])
        cert (proxy [java.security.cert.X509Certificate] [])
        request {:server-port    8080
                 :server-name    "foobar"
                 :remote-addr    "127.0.0.1"
                 :uri            "/foo"
                 :query-string   "a=b"
                 :scheme         :http
                 :request-method :get
                 :protocol       "HTTP/1.1"
                 :headers        {"X-Client" ["Foo", "Bar"]
                                  "X-Server" ["Baz"]
                                  "X-Capital-I" ["Qux"]}
                 :content-type   "text/plain"
                 :content-length 10
                 :character-encoding "UTF-8"
                 :servlet-context-path "/foo"
                 :ssl-client-cert cert
                 :body            body}
          response (atom {})]
    (letfn [(handler [r]
             (are [k v] (= (r k) v)
               :server-port    8080
               :server-name    "foobar"
               :remote-addr    "127.0.0.1"
               :uri            "/foo"
               :query-string   "a=b"
               :scheme         :http
               :request-method :get
               :headers        {"x-client" "Foo,Bar"
                                "x-server" "Baz"
                                "x-capital-i" "Qux"}
               :content-type   "text/plain"
               :content-length 10
               :character-encoding "UTF-8"
               :servlet-context-path "/foo"
               :ssl-client-cert cert
               :body            body)
             {:status 200, :headers {}})]
      (testing "request"
        (run-servlet handler request response))
      (testing "mapping request header names to lower case"
        (with-locale (Locale. "tr")
          (run-servlet handler request response))))
    (testing "response"
      (letfn [(handler [r]
               {:status  200
                :headers {"Content-Type" "text/plain"
                          "X-Server" "Bar"}
                :body    nil})]
        (run-servlet handler request response)
        (is (= (@response :status) 200))
        (is (= (@response :content-type) "text/plain"))
        (is (= (get-in @response [:headers "X-Server"]) "Bar"))))))

