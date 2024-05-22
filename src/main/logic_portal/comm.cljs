(ns logic-portal.comm
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require  [cljs-http.client :as http]
             [cljs.core.async :refer [<!]]
             [reagent.core :as r]))

(defonce base "http://34.238.208.202:5001/")
(defonce assertions (r/atom nil))
(defonce assertion-to-edit (r/atom {}))

(defn get-req
  ([path a]
   (go (let [response (<! (http/get path {:with-credentials? false}))]
         (reset! a (:body response)))))
  ([path k a]
   (go (let [response (<! (http/get path {:with-credentials? false}))]
         (reset! a (get-in response [:body k]))))))

(defn post-req
  ([path m]
   (go (let [response (<! (http/post (str base path)
                                    {:with-credentials? false
                                     :edn-params m}))]
        (prn (:status response))
        (prn (:body response))))))

(defn all-assertions []
  (get-req (str base "all-assertions/") assertions))

(defn retrieve-assertion [k a]
  (get-req (str base "get-assertion/" k) a))

(defn submit-assertion [m]
  (post-req "add-assertion/" m))

(defn relate-assertion [m]
  (post-req "relate-assertion/" m))
