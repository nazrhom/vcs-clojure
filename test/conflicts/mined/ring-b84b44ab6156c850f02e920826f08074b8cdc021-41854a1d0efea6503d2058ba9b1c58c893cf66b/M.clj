<<<<<<< A.clj
(defproject ring/ring-httpcore-adapter "0.2.6"
||||||| O.clj
(defproject ring/ring-httpcore-adapter "0.2.5"
=======
(defproject ring/ring-httpcore-adapter "0.3.0-beta1"
>>>>>>> B.clj
  :description "Ring HttpCore adapter."
  :url "http://github.com/mmcgrana/ring"
<<<<<<< A.clj
  :dependencies [[ring/ring-core "0.2.6"]
||||||| O.clj
  :dependencies [[ring/ring-core "0.2.5"]
=======
  :dependencies [[ring/ring-core "0.3.0-beta1"]
>>>>>>> B.clj
                 [org.apache.httpcomponents/httpcore "4.0.1"]
                 [org.apache.httpcomponents/httpcore-nio "4.0.1"]]
  :dev-dependencies [[lein-clojars "0.6.0"]])
