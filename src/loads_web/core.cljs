(ns loads-web.core
    (:require 
      [ajax.core :refer [GET]]
      [org.clojars.lynch.loads.io :as io]
      [org.clojars.lynch.loads.types :as t]
      [org.clojars.lynch.loads.errors :as e]
      [reagent.core :as reagent :refer [atom]]
      [clojure.string :as string]
      [clojure.spec.alpha :as s]
      ))

(enable-console-print!)

(println "App initialize!")

;;--------------------------------------------------------------------------
;; State 
;;--------------------------------------------------------------------------

(defonce app-state (atom nil))

;;--------------------------------------------------------------------------
;; Components 
;;--------------------------------------------------------------------------

(defn Line [name samples]
  (let [sample1 (first samples)
        sample2 (second samples)
        sign    #(if (pos? %) "+" "-")]
      [:tr
       [:td (str name)]
       [:td (sign sample1)]
       [:td (Math/abs sample1)]
       [:td (sign sample2)]
       [:td (Math/abs sample2)]]))

(defn Section [lines]
  [:<> 
       [:tr
        [:td {:colSpan 5} "Секция"]]
       (into [:<>] (for [i lines] 
                     ^{:key i} 
                     (let [{:keys [name samples]} i]
                       (Line name samples))))])

(defn Station [name sections]
  [:div.card
       [:div.card-header
        [:p.card-header-title
         (str name)]]
       [:div.card-content
        [:div.content
         [:table.table
          [:thead
           [:tr
            [:td "Линия"]
            [:td "+/-"]
            [:td "Нагрузка"]
            [:td "+/-"]
            [:td "Нагрузка"]]]
          (into [:tbody] (for [i sections] 
                           ^{:key i} 
                           (let [{:keys [lines]} i]
                             (Section lines))))]]]])

;;--------------------------------------------------------------------------
;; Protocols 
;;--------------------------------------------------------------------------

(defprotocol IRenderLoads
  (render [this]))

;;--------------------------------------------------------------------------
;; Errors 
;;--------------------------------------------------------------------------

(defrecord IOError [msg-key])

(def errors-map
  {:error-on-data-loading "Ошибка при загрузке данных с сервера"
   :require-empty-string "Строка не может быть пустой"
   :require-integer-value "Необходимо целочисленное значение"
   :require-array-of-integers "Необходим массив целых чисел"
   :error-in-line-data "Ошибка в данных линии"
   :error-in-section-data "Ошибка в данных секции"
   :error-in-station-data "Ошибка в данных станции"
   :default-message "Ошибка в входных данных"})

(extend-type IOError
  IRenderLoads
  (render [this]
    (let [error-msg ((:msg-key this) errors-map (:default-message errors-map))]
      [:div.card
       [:div.card-header
        [:p.card-header-title "Ошибка"]]
       [:div.card-content
        [:div.content
         [:p error-msg]]]])))

(extend-type e/SError
  IRenderLoads
  (render [this]
    (let [error-msg ((:key this) errors-map (:default-message errors-map))
          path      (:path this)
          path-msg  (string/join " -> " (remove string/blank? path))
          name      (first path)
          error-title (str (when name (str name " ")) "Ошибка")]
      [:div.card
       [:div.card-header
        [:p.card-header-title error-title]]
       [:div.card-content
        [:div.content
         [:p (str error-msg ": " path-msg)]]]])))

;;--------------------------------------------------------------------------
;; Types and protocols extensions
;;--------------------------------------------------------------------------

(extend-type t/Station
  IRenderLoads
  (render [this]
    (let [{:keys [name sections]} this]
      (Station name sections))))

(extend-protocol IRenderLoads
  List
  LazySeq
  (render [this]
    (into [:div] (for [item this] ^{:key item} (render item)))))

(extend-type nil
  IRenderLoads
  (render [this]
    nil))

;;--------------------------------------------------------------------------
;; Fetching data
;;--------------------------------------------------------------------------

(GET "/stations.json" 
       {:response-format :json
        :keywords? true
        :handler (fn [arg]
                   (reset! app-state (map #(io/from-map ::t/->Station %) arg))
                   (.log js/console "Load data completed ..."))
        :error-handler (fn [_]
                         (reset! app-state (->IOError :error-on-data-loading))
                         (.log js/console (str "Error on data loading")))})

;;--------------------------------------------------------------------------
;; Main app 
;;--------------------------------------------------------------------------

(defn main-app []
  [:div
   (render @app-state)])

(reagent/render-component [main-app]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(comment
  (GET "/stations2.json" 
       {:response-format :json
        :keywords? true
        :handler (fn [arg]
                   (reset! app-state (map #(io/from-map ::t/->Station %) arg))
                   #_(reset! app-state (io/from-map ::t/->Stations arg))
                   (.log js/console "Load data completed ..."))
        :error-handler (fn [_]
                         (reset! app-state (->IOError :error-on-data-loading))
                         (.log js/console (str "Error on data loading")))})
  (type (io/from-map ::t/->Line {:name "A" :samples [1 2]}))
  (io/from-map ::t/->Line {:name "" :samples [1 2]})
  (let [data {:name "" :samples [1 2]}
        conf (s/conform ::t/->Line data)]
    (if (= conf ::s/invalid)
      (->> data
           (s/explain-data ::t/->Line)
           #_(e/get-error-messages)
           #_(map #(e/->SError (last %) (first %))))))
  (deref app-state)
  (render @app-state)
  )
