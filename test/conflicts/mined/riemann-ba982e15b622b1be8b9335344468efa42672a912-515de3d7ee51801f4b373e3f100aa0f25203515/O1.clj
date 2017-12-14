(ns riemann.config
  "Riemann config files are eval'd in the context of this namespace. Includes
  streams, client, email, logging, and graphite; the common functions used in
  config. Provides a default core and functions ((tcp|udp)-server, streams,
  index, reinject) which operate on that core."
  (:import (java.io File))
  (:require [riemann.core :as core]
            [riemann.common :as common :refer [event]]
            [riemann.service :as service]
            [riemann.transport.tcp        :as tcp]
            [riemann.transport.udp        :as udp]
            [riemann.transport.websockets :as websockets]
            [riemann.transport.sse        :as sse]
            [riemann.transport.graphite   :as graphite]
            [riemann.logging :as logging]
            [riemann.folds :as folds]
            [riemann.pubsub :as pubsub]
            [riemann.graphite :as graphite-client]
            [riemann.logstash :as logstash-client]
            [clojure.tools.nrepl.server :as repl]
            [riemann.repl]
            [riemann.index]
            [riemann.test :as test :refer [tap io tests]])
  (:use clojure.tools.logging
        [clojure.java.io :only [file]]
        [riemann.client :only [udp-client tcp-client multi-client]]
        riemann.email
        riemann.sns
        [riemann.plugin  :only [load-plugin load-plugins]]
        [riemann.time :only [unix-time linear-time once! every!]]
        [riemann.pagerduty :only [pagerduty]]
        [riemann.campfire :only [campfire]]
        [riemann.kairosdb :only [kairosdb]]
        [riemann.librato :only [librato-metrics]]
        [riemann.nagios :only [nagios]]
        [riemann.opentsdb :only [opentsdb]]
        [riemann.influxdb :only [influxdb]]
        [riemann.hipchat :only [hipchat]]
        [riemann.slack :only [slack]]
        [riemann.stackdriver :only [stackdriver]]
        riemann.streams))

