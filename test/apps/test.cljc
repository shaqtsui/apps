(ns apps.test
  (:require [#?(:clj clojure.test
                :cljs cljs.test) :refer [deftest is testing]]
            [etaoin.api :as ea]
            [etaoin.keys :as ek]))

(deftest deps-test
  (testing "Deps..."
    #?@(:clj
        [(is (= nil (require 'apps.cms)))
         (is (= nil (require 'apps.matrix)))
         (is (= nil (require 'apps.tools)))
         (is (= nil (require 'apps.bootstrap.cljs)))
         (is (= nil (require 'apps.image)))
         (is (= nil (require 'apps.nrepl-cider)))]
        :cljs
        [(is (= nil (require 'apps.func-plot)))])))



(deftest browser-test
  (testing "CMS"
    #?@(:clj
        [(let [driver (ea/chrome)])])))
