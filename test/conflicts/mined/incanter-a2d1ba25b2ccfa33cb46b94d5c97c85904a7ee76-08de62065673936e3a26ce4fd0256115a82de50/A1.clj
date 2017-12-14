(ns ^{:doc "This is the core statistical library for Incanter.
            It provides probability functions (cdf, pdf, quantile),
            random number generation, statistical tests, basic
            modeling functions, similarity/association measures,
            and more.

            This library is built on Parallel Colt 
            (http://sites.google.com/site/piotrwendykier/software/parallelcolt),
            an extension of the Colt numerics library 
            (http://acs.lbl.gov/~hoschek/colt/).
            "
       :author "David Edgar Liebke and Bradford Cross"}
  incanter.stats
  (:import (cern.colt.list.tdouble DoubleArrayList)
           (cern.jet.random.tdouble Gamma Beta Binomial ChiSquare DoubleUniform
                                    Exponential NegativeBinomial Normal Poisson
                                    StudentT)
           (cern.jet.random.tdouble.engine DoubleMersenneTwister)
           (cern.jet.stat.tdouble DoubleDescriptive
                                  Probability)
           (incanter.random Weibull))
  (:use [incanter.probability :only [gt lt binary]])
  (:use [incanter.transformations :only [map-map same-length? sort-map]])
  (:use [incanter.internal :only [tree-comp-each]])
  (:use [clojure.contrib.map-utils :only [deep-merge-with]])
  (:use [clojure.set :only [difference intersection union]])
  (:use [incanter.core :only (abs plus minus div mult mmult to-list bind-columns
                              gamma pow sqrt diag trans regularized-beta ncol
                              nrow identity-matrix decomp-cholesky decomp-svd
                              matrix length sum sum-of-squares sel matrix?
                              cumulative-sum solve vectorize bind-rows)]))

