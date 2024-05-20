(ns logic-portal.app
  (:require ["react" :as react]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [reagent.dom.client :as rdomc]
            [reagent-forms.core :refer [bind-fields]]
            [clojure.string :as str]
            [logic-portal.comm :as comm]))

(defonce title "Welcome to Assertive Accounting.")
(defonce mode-atom (r/atom :welcome))

(defn atom-input
  ([value]
   [:input {:type "text" :value @value
            :on-change #(reset! value (-> % .-target .-value))}])
  ([value path]
   [:input {:type "text" :value (get-in @value path)
            ;; :style  {:width "50%" :height 200}
            :on-change #(swap! value assoc-in path (-> % .-target .-value))}])
  ([value path style]
   [:input {:type "textarea" :value (get-in @value path)
            :style style :wrap "soft"
            :on-change #(swap! value assoc-in path (-> % .-target .-value))}])
  ([value path style f]
   [:input {:type "text" :value (get-in @value path)
            :style style
            :on-change #(do (swap! value assoc-in path (-> % .-target .-value))
                            f)}]))

(defn submit-button
  ([text a v]
   [:div [:input.btn {:type "button" :value text
                      :on-click #(reset! a v)}]])
  ([text a v f]
   (do (f) (submit-button text a v)))
  ([text f]
   [:div [:input.btn {:type "button" :value text
                      :on-click f}]]))

(defn row [label input]
  [:div.row
   [:div.col [:label label]]
   [:div.col input]])

(defn welcome-screen []
  (let [typed-user (r/atom "")]
    [:div#welcome-screen
     [:h1 title]
     [:p "This application demonstrates assertive accounting."]
     [submit-button "View assertions" mode-atom :view-assert comm/all-assertions]
     [submit-button "View transactions" mode-atom :view-transactions]]))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn arrange-values [value form]
  [:tr {:id value} [:td value]
   [:td [:input.btn {:type "button" :value "Remove value."
                     :on-click (fn [] (swap! form update :assertion/required-value (fn [coll] (remove #(= % value) coll))))}]]])

(defn edit-assert [form]
  [:div
   [:div "Enter key for the assertion."
    [:p]
    [:textarea {:value (name (or (:assertion/keyword @form) ""))
                :rows "1"
                :on-change #(swap! form assoc :assertion/keyword (keyword (-> % .-target .-value)))}]]
   [:div "Enter text for the assertion."
    [:p]
    [:textarea {:value (:assertion/description @form)
                :rows "1"
                :cols "100"
                :style {:cols "200" :rows "200"}
                :on-change #(swap! form assoc :assertion/description (-> % .-target .-value))}]]
   (r/with-let [new-value (r/atom "")]
     [:div "Enter an (optional) value to associate with the assertion."
      [atom-input form [:assertion/required-value] ]
      #_[:input.btn {:type "button" :value "Add value."
                   :on-click (fn [] (swap! form update :assertion/required-value (fnil conj []) @new-value)
                               (reset! new-value ""))}]
      #_[into [:div]
       (mapv #(arrange-values % form) (:assertion/required-value @form))]])
   [:input.btn {:type "button" :value "Submit assertion."
                  :on-click (fn []
                              (do (comm/submit-assertion @form)
                                  (comm/all-assertions)
                                  (reset! form {})))}]
   
   [submit-button "Return to assertions." mode-atom :view-assert]
   [:div (str @form)]])

(defn select-assertions [[id s]]
  [:option {:value id} s])

(defn relate-assert
  ([]
   [:div#relate-assert
    [:h1 "Relate assertions."]
    [:p "Select assertion:"]
    ])
  ([a]
   [:div#relate-assert
    [:h1 "Relate assertions."]
    [:p "Relating assertion: "(:assertion/description @a)]
    [into [:select
           {:id "assert-select"
            :on-change #(when (not (= "" (.. % -target -value)))
                          (swap! a assoc :relation/keyword (.. % -target -value)))}
           [:option {:value nil} ""]]
     (map select-assertions @comm/assertions)]
    [into [:select
           {:id "relation-select"
            :on-change #(when (not (= "" (.. % -target -value)))
                          (swap! a assoc :relation/type (.. % -target -value)))}
           [:option {:value nil} ""]
           [:option {:value :parent} "parent"]
           [:option {:value :child} "child"]
           [:option {:value :conflict} "conflict"]]
     ]
    [:input.btn {:type "button" :value "Relate assertion."
                 :on-click (fn []
                             (do (comm/relate-assertion (select-keys @a [:assertion/keyword :relation/keyword
                                                                         :relation/type]))))}]
       [submit-button "Return to assertions." mode-atom :view-assert]
    [:div (str @a)]
    ]))

(defn arrange-assertions [[k v] a]
  [:tr {:id k} [:td "..."] [:td v]
   [:td [:input.btn {:type "button" :value "Edit assertion."
                     :on-click (fn [] (do
                                        (reset! mode-atom :edit-assert)
                                        (comm/retrieve-assertion k a)))}]]
   [:td [:input.btn {:type "button" :value "Relate assertion."
                     :on-click (fn [] (do
                                        (reset! mode-atom :relate-assert)
                                        (comm/retrieve-assertion k a)))}]]])


(defn view-assert []
  (r/with-let [form (r/atom {})] 
    [:div#view-assert
     [:h1 "View assertions."]
     [:p "This transaction..."]
     [:table [into [:tbody] (mapv #(arrange-assertions % comm/assertion-to-edit) @comm/assertions)]]
     [:p]
     [submit-button "Add assertion" mode-atom :add-assert]
     [submit-button "Return to home" mode-atom :welcome]]))


(defn view-transactions []
  [:div#view-transactions
   [:h1 "View transactions."]
   [:p "This page lists all transactions and allows filtering."]
   [submit-button "Return to home" mode-atom :welcome]])

(defn app []
  (case @mode-atom
    :view-assert [view-assert]
    :add-assert [edit-assert (r/atom {})]
    :edit-assert [edit-assert comm/assertion-to-edit]
    :relate-assert [relate-assert comm/assertion-to-edit]
    :view-transactions [view-transactions]
    [welcome-screen]))

(defonce root (rdomc/create-root (.getElementById js/document "root")))
(defn init []
  (.render root (r/as-element [app])))

(defn stop []
  (js/console.log "Stopping..."))

(defn ^:dev/after-load re-render []
  (init))
