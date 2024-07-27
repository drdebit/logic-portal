(ns logic-portal.comm
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require  [cljs-http.client :as http]
             [cljs.core.async :refer [<! >! chan]]
             [reagent.core :as r]))

(defonce base "http://choochoo.dyn.gsu.edu:5001/")
(defonce assertions (r/atom nil))
(defonce assertion-to-edit (r/atom {}))
(defonce transaction-to-edit (r/atom {}))

(def c (chan))
(go-loop []
  (let [m (<! c)]
    (case (:type m)
      :get (reset! (:atom m) (:body
                              (<! (http/get (str base (:path m))
                                            {:with-credentials? false}))))
      :post (:status (<! (http/post (str base (:path m))
                                    {:with-credentials? false
                                     :edn-params (:data m)})))
      :reset (reset! (:atom m) (:new-value m))))
  (recur))

(defn top-level-assertions [av]
  (filter #(not (contains? % :assertion/depends-on)) @av))

(defn child-assertions [av pid]
  (let [id->assertion (fn [id] (filter #(= (:db/id %) id) @av))
        full-assertion (first (id->assertion pid))]
    (->> (:assertion/dependent full-assertion)
         (map :db/id)
         (map id->assertion)
         flatten)))

(defn parent-ids [id]
  (let [vm->s (fn [vm] (apply hash-set (map :db/id vm)))]
    (mapv :db/id (filter #(contains? (vm->s (:assertion/dependent %)) id) @assertions))))

(defn get-req
  ([path a]
   (go (let [response (<! (http/get path {:with-credentials? false}))]
          (reset! a (:body response)))))
  ([path k a]
   (go (let [response (<! (http/get path {:with-credentials? false}))]
         (reset! a (get-in response [:body k]))))))

(defn post-req
  ([path m]
   (go (let [response  (<! (http/post (str base path)
                                    {:with-credentials? false
                                     :edn-params m}))]
        (prn (:status response))
        (prn (:body response))))))

(defn all-assertions []
  (go (>! c {:path "all-assertions/"
             :type :get
             :atom assertions})))

(defn all-relations [kw k a]
  (get-req (str base "all-relations/" kw) k a))

(defn retrieve-assertion [k a]
  (get-req (str base "get-assertion/" k) a))

(defn retrieve-assertion-with-relations [k a]
  (go (let [assertion-response (<! (http/get (str base "get-assertion/" k)
                                             {:with-credentials? false}))
            relation-response (<! (http/get (str base "all-relations/" k)
                                            {:with-credentials? false}))]
        (reset! a (:body assertion-response))
        (swap! a assoc :relations (:body relation-response)))))

(defn submit-assertion [m]
  (post-req "add-assertion/" m))

(defn retract-assertion [m]
  (post-req "retract-assertion/" m))

(defn change-relate-and-refresh [post-path m k a]
  (go (let [post-response (<! (http/post (str base post-path)
                                         {:with-credentials? false
                                          :edn-params m}))
            get-response (<! (http/get (str base "get-assertion/" k) {:with-credentials? false}))
            relation-response (<! (http/get (str base "all-relations/" k)
                                            {:with-credentials? false}))]
        (reset! a (:body get-response))
        (swap! a assoc :relations (:body relation-response)))))

;; Try a go-loop to pair responses with atoms iteratively
;; https://clojuredocs.org/clojure.core.async/go-loop

;; What can I pass?
;; keyword, which links to a vector containing a path and an atom.
;; vector of keywords, each of which links to a vector containing a path and an atom.
;; keyword and data, which is either another keyword (to add to the path) or a map.
;; vector of vectors, in which each vector contains a keyword and data.

(def request-map {:submit-assertion "add-assertion/"
                  :all-assertions ["all-assertions/" assertions]
                  :all-relations ["all-relations/"]})

(defn sequential-requests
  ([rs]
   (cond
     (keyword? rs) (let [rv (get request-map rs)]
                     (get-req (str base (first rv)) (second rv))) 
     (vector? rs) (go-loop [r (first rs)]
                    (cond
                      (nil? r) nil
                      (keyword? r) (do (sequential-requests r) (recur (next rs)))
                      (vector? r) (do (apply sequential-requests r) (recur (next rs)))))
     :else nil))
  ([rs data]
   (let [rv (get request-map rs)]
    (cond
      (keyword? data) (get-req (str base (first rv) data) (second rv))
      (map? data) (post-req (str base (first rv)) data)))))

;; Test channels
;; (defn test-req [path]
;;   (go (>! c (<! (http/get path {:with-credentials? false})))))

;; (go-loop []
;;   (let [x (<! c)
;;         resolve-fn (fn
;;                      ([channel]
;;                       (println (<! channel)))
;;                      ([channel a]
;;                       (reset! a (<! channel))))]
;;     (println x))
;;   (recur))
