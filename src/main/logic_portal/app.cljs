(ns logic-portal.app
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require ["react" :as react]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [reagent.dom.client :as rdomc]
            [reagent-forms.core :refer [bind-fields]]
            [clojure.string :as str]
            [cljs.core.async :refer [<! >!]]
            [logic-portal.comm :as comm]
            [logic-portal.simulation :as sim]))

(defonce title "Welcome to Assertive Accounting.")
(defonce mode-atom (r/atom :welcome)) 

(defn id->assertion [id]
  (first (filter #(= id (:db/id %)) @comm/assertions)))

(defn id->kw [id]
  (:assertion/keyword (id->assertion id)))

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
                      :on-click
                      (fn []
                        (go (>! comm/c {:type :reset
                                        :atom a
                                        :new-value v})))}]])
  ([text a v f]
   [:div [:input.btn {:type "button" :value text
                      :on-click
                      (fn []
                        (go
                          (f)
                          (>! comm/c {:type :reset
                                      :atom a
                                      :new-value v})))}]])
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
     [submit-button "Run simulation" mode-atom :simulation comm/all-assertions]
     [submit-button "View assertions" mode-atom :view-assert comm/all-assertions]
     [submit-button "View transactions" mode-atom :view-transactions comm/all-assertions]]))

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
   [:div "Enter an (optional) value and data type (one of \"text\", \"number\", or \"reference\") to associate with the assertion."
    [atom-input form [:assertion/require-value :required-value/description]]
    [:textarea {:value (name (or (get-in @form [:assertion/require-value
                                                :required-value/data-type]) ""))
                :rows "1"
                :on-change #(swap! form assoc-in [:assertion/require-value
                                                  :required-value/data-type] (keyword (-> % .-target .-value)))}]
    #_[atom-input form [:assertion/required-value :required-value/data-type]]]
   [:input.btn {:type "button" :value "Submit assertion."
                :on-click (fn []
                            (go (>! comm/c {:path "add-assertion/"
                                            :type :post
                                            :data @form})
                                (>! comm/c {:type :reset
                                            :atom form
                                            :new-value {}})))}]
   [:input.btn {:type "button" :value "Submit assertion and add relations."
                :on-click (fn []
                            (go (>! comm/c {:path "add-assertion/"
                                            :type :post
                                            :data @form})
                                (>! comm/c {:type :get
                                            :atom comm/assertion-to-edit
                                            :path (str "get-assertion/" (:assertion/keyword @form))})
                                (>! comm/c {:type :reset
                                            :atom mode-atom
                                            :new-value :relate-assert})))}]
   [submit-button "Return to assertions." mode-atom :view-assert comm/all-assertions]
   [:div (str @form)]])

(defn select-assertions [{id :assertion/keyword
                          s :assertion/description}]
  [:option {:value id} s])

(defn retrieve-relations [a]
  (for [k [:assertion/depends-on :assertion/dependent :assertion/conflicts-with]]
    (doall
     (for [r (get @a k)]
       (let [relation (id->assertion (:db/id r))
             rk (:assertion/keyword relation)
             tk (:assertion/keyword @a)
             rt (case k
                  :assertion/depends-on :parent
                  :assertion/dependent :child
                  :assertion/conflicts-with :conflict)]
         ^{:key rk}
         [:tr {:id rk}
          [:td "..."] [:td (:assertion/description relation)]
          [:td " is a " ] [:td rt]
          [:td [:input.btn {:type "button" :value "Remove relation."
                            :on-click
                            (fn []
                              (do (comm/change-relate-and-refresh
                                   "unrelate-assertion/"
                                   {:assertion/keyword rk
                                    :relation/keyword tk
                                    :relation/type rt}
                                   tk
                                   a)))}]]])))))

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
           {:id "relation-select"
            :on-change #(when (not (= "" (.. % -target -value)))
                          (swap! a assoc :relation/type (keyword (.. % -target -value))))}
           [:option {:value nil} ""]
           [:option {:value :parent} "is a parent of"]
           [:option {:value :child} "is a child of"]
           [:option {:value :conflict} "conflicts with"]]
     ]
    [into [:select
           {:id "assert-select"
            :on-change #(when (not (= "" (.. % -target -value)))
                          (swap! a assoc :relation/keyword (keyword (.. % -target -value))))}
           [:option {:value nil} ""]]
     (map select-assertions
          (filter #(and (not= (:assertion/keyword @a) (:assertion/keyword %))
                        (not (contains? (apply hash-set (map id->kw (:relations @a))) (:assertion/keyword %))))
                  @comm/assertions))]

    [:input.btn {:type "button" :value "Relate assertion."
                 :on-click (fn []
                             (do (comm/change-relate-and-refresh
                                  "relate-assertion/"
                                  (select-keys @a [:assertion/keyword
                                                   :relation/keyword
                                                   :relation/type])
                                  (:assertion/keyword @a)
                                  a)
                                 
                                 (set! (.. (.getElementById js/document "relation-select") -value) "")
                                 (set! (.. (.getElementById js/document "assert-select") -value) "")))}]
    [:div
     [:p "Existing relations"]
     [:table [into [:tbody] (retrieve-relations a)]]
     [:p]]
    [submit-button "Return to assertions." mode-atom :view-assert comm/all-assertions]
    [:div (str @a)]
    ]))

(defn arrange-assertions [{k :assertion/keyword
                           v :assertion/description} a]
  [:tr {:id k} [:td "..."] [:td v]
   [:td [:input.btn {:type "button" :value "Edit assertion."
                     :on-click (fn [] (do
                                        (reset! mode-atom :edit-assert)
                                        (comm/retrieve-assertion k a)))}]]
   [:td [:input.btn {:type "button" :value "Relate assertion."
                     :on-click (fn [] (do
                                        (reset! mode-atom :relate-assert)
                                        (comm/retrieve-assertion-with-relations k a)))}]]
   [:td [:input.btn {:type "button" :value "Remove assertion"
                     :on-click (fn []
                                 (go (>! comm/c {:path "retract-assertion/"
                                                 :type :post
                                                 :data {:assertion/keyword k}})
                                     (comm/all-assertions)))}]]])

(defonce graph-state (r/atom '(+ 1 2 (- 4 2) (/ 123 3) (inc 25))))

(defn view-assert []
  (r/with-let [form (r/atom {})] 
    [:div#view-assert
     [:h1 "View assertions."]
     [:p "This transaction..."]
     [:table [into [:tbody] (mapv #(arrange-assertions % comm/assertion-to-edit)
                                  @comm/assertions)]]
     [:p]
     [submit-button "Add assertion" mode-atom :add-assert]
     [submit-button "Return to home" mode-atom :welcome]]))


(defn view-transactions []
  [:div#view-transactions
   [:h1 "View transactions."]
   [:p "This page lists all transactions and allows filtering."] 
   [submit-button "Add transaction" mode-atom :add-transaction]
   [submit-button "Return to home" mode-atom :welcome]])

(defn assertion-select-row [assertion form remove?]
  (let [[remove-key add-key button-text] (if remove?
                                           [:related-assertions :relatable-assertions "Remove"]
                                           [:relatable-assertions :related-assertions "Add"])
        {id :db/id
         desc :assertion/description} assertion]
    [:tr {:id id} [:td "..."] [:td desc]
     [:td [:input.btn {:type "button" :value (str button-text " assertion.")
                       :on-click (fn []
                                   (do
                                     (swap! form
                                            update
                                            remove-key
                                            (fn [v]
                                              (vec
                                               (remove #(= (:db/id %) id) v))))
                                     (swap! form update add-key #(conj % assertion))))}]]
     [:td] [:td]]))

(defn assertion-option [{id :db/id
                         s :assertion/description}]
  [:option {:value id} s])

(defn required-value-prompts [i form]
  (when-let [cid (get (:related-assertions @form) i)]
    (let [rv-fun (fn [value]
                   (swap! form
                          update
                          :related-values
                          (fnil (fn [v]
                                  (assoc v i value)) [])))
          a (id->assertion cid)]
      (if (contains? a :assertion/require-value)
        [:div "Enter " (get-in a [:assertion/require-value :required-value/description])
         [:textarea {:value (get (:related-values @form) i)
                     :rows "1"
                     :cols "100"
                     :style {:cols "200" :rows "200"}
                     :on-change #(rv-fun (-> % .-target .-value))}]] 
        (rv-fun "")))))

(defn next-level-assertions [form n]
  (map (fn [i]
         (let [ras (get (:related-assertions @form) i)
               cas (comm/child-assertions comm/assertions ras)]
           (when (not-empty cas)
             [:div "Select a next-level assertion."
              [into [:select {:id (str "next-select" n)
                              :key n
                              :on-change #(when (not (= "" (.. % -target -value)))
                                            (swap! form update :related-assertions
                                                   (fn [v]
                                                     (assoc v (+ i 1) (js/parseInt (.. % -target -value))))))}]
               (map assertion-option (into [{:db/id "" :assertion/description ""}] cas))]
              (required-value-prompts (+ i 1) form)]))) (range n)))

(defn add-transaction [form]
  (r/with-let [show-list (r/atom false)] 
    [:div#add-transaction
     [:h1 "Add a transaction"]
     [:p]
     [:div "Enter an optional description for your transaction."
      [:p]
      [:textarea {:value (:transaction/description @form)
                  :rows "1"
                  :cols "100"
                  :style {:cols "200" :rows "200"}
                  :on-change #(swap! form
                                     assoc
                                     :transaction/description
                                     (-> % .-target .-value))}]]
     [:div "Select a top-level assertion."
      [into [:select {:id "top-select" 
                      :on-change #(when (not (= "" (.. % -target -value)))
                                    (do (swap! form assoc :related-assertions
                                               [(js/parseInt (.. % -target -value))])
                                        (swap! form assoc :related-values [])
                                        (set! (.. (.getElementById js/document "next-select1") -value) "")))}]
       (map assertion-option (into [{:db/id "" :assertion/description ""}] (comm/top-level-assertions comm/assertions)))]
      (required-value-prompts 0 form)]
     (when-let [pa (get (:related-assertions @form) 0)]
       (next-level-assertions form (count (:related-assertions @form))))
     #_[:div
        [submit-button
         (if @show-list
           "Hide list of assertions."
           "Show list of assertions.")
         show-list
         (if @show-list false true)]
        [:div [:p]
         (when @show-list
           [:table {:style {:float "left"}}
            [:caption "Assertions to add:"]
            [into [:tbody] (mapv #(assertion-select-row % form false) (:relatable-assertions @form))]])
         (when (and @show-list (not (empty? (:related-assertions @form))))
           [:table {:style {:float "left" :white-space "nowrap"}}
            [:caption "Assertions added:"]
            [into [:tbody] (mapv #(assertion-select-row % form true) (:related-assertions @form))]])]]]))

(defn simulation []
  (r/with-let [stage (r/atom :start)
               books (r/atom {})]
    [:div#simulation {:style {:width "75%"
                              :white-space "pre-wrap"}}
     [sim/simulation]]))

(defn app []
  (case @mode-atom
    :view-assert [view-assert]
    :add-assert [edit-assert (r/atom {})]
    :edit-assert [edit-assert comm/assertion-to-edit]
    :relate-assert [relate-assert comm/assertion-to-edit]
    :view-transactions [view-transactions]
    :add-transaction [add-transaction (r/atom {:relatable-assertions @comm/assertions
                                               :related-assertions []})]
    :edit-transaction [add-transaction comm/transaction-to-edit]
    :simulation [simulation]
    [welcome-screen]))

(defonce root (rdomc/create-root (.getElementById js/document "root")))
(defn init []
  (.render root (r/as-element [app])))

(defn stop []
  (js/console.log "Stopping..."))

(defn ^:dev/after-load re-render []
  (init))
