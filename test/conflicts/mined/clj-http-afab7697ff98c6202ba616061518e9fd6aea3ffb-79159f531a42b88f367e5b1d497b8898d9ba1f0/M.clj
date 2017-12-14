(defproject clj-http "0.3.3"
  :description "A Clojure HTTP library wrapping the Apache HttpComponents client."
  :url "https://github.com/dakrone/clj-http/"
  :repositories {"sona" "http://oss.sonatype.org/content/repositories/snapshots"}
  :warn-on-reflection false
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.apache.httpcomponents/httpclient "4.1.2"]
                 [org.apache.httpcomponents/httpmime "4.1.2"]
                 [commons-codec "1.5"]
                 [commons-io "2.1"]
                 [slingshot "0.10.2"]
<<<<<<< A.clj
                 [cheshire "3.0.0"]]
  :multi-deps {"1.2.1" [[org.clojure/clojure "1.2.1"]
                        [org.apache.httpcomponents/httpclient "4.1.2"]
                        [org.apache.httpcomponents/httpmime "4.1.2"]
                        [commons-codec "1.5"]
                        [commons-io "2.1"]
                        [slingshot "0.10.2"]
                        [cheshire "3.0.0"]]
               "1.4.0" [[org.clojure/clojure "1.4.0-beta3"]
                        [org.apache.httpcomponents/httpclient "4.1.2"]
                        [org.apache.httpcomponents/httpmime "4.1.2"]
                        [commons-codec "1.5"]
                        [commons-io "2.1"]
                        [slingshot "0.10.2"]
                        [cheshire "3.0.0"]]}
  :dev-dependencies [[ring/ring-jetty-adapter "1.0.2"]
                     [ring/ring-devel "1.0.2"]
                     [lein-multi "1.1.0"]]
||||||| O.clj
                 [cheshire "2.2.2"]]
  :multi-deps {"1.2.1" [[org.clojure/clojure "1.2.1"]
                        [org.apache.httpcomponents/httpclient "4.1.2"]
                        [org.apache.httpcomponents/httpmime "4.1.2"]
                        [commons-codec "1.5"]
                        [commons-io "2.1"]
                        [slingshot "0.10.2"]
                        [cheshire "2.2.2"]]
               "1.4.0" [[org.clojure/clojure "1.4.0-beta3"]
                        [org.apache.httpcomponents/httpclient "4.1.2"]
                        [org.apache.httpcomponents/httpmime "4.1.2"]
                        [commons-codec "1.5"]
                        [commons-io "2.1"]
                        [slingshot "0.10.2"]
                        [cheshire "2.2.2"]]}
  :dev-dependencies [[ring/ring-jetty-adapter "1.0.2"]
                     [ring/ring-devel "1.0.2"]
                     [lein-multi "1.1.0"]]
=======
                 [cheshire "2.2.2"]]
  :profiles {:dev {:dependencies [[ring/ring-jetty-adapter "1.0.2"]
                                  [ring/ring-devel "1.0.2"]]}
             :1.2 {:dependencies [[org.clojure/clojure "1.2.1"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0-beta3"]]}}
  :aliases {"all" ["with-profile" "dev,1.2:dev:dev,1.4"]}
>>>>>>> B.clj
  :test-selectors {:default  #(not (:integration %))
                   :integration :integration
                   :all (constantly true)}
  :checksum-deps true)
