(ns loads-web.core
  (:require 
    [ajax.core :refer [GET]]
    [clojure.string :as string]
    [reagent.core :as reagent :refer [atom]]
    ;;
    [org.clojars.lynch.loads.io :as io]
    [org.clojars.lynch.loads.types :as t]
    [org.clojars.lynch.loads.errors :as e]
    [org.clojars.lynch.loads.fake :refer [recalc-load]]
    [org.clojars.lynch.loads.calc :refer [recalc-samples]]
    ))

(enable-console-print!)

(println "App initialize!")

;;--------------------------------------------------------------------------
;; State and handlers
;;--------------------------------------------------------------------------

(defonce station-list (atom nil))
(defonce current-station (atom nil))

(defn set-current-station! [name]
  (let [station (first (filter #(= (:name %) name) @station-list))]
    (reset! current-station station)))

(defn reset-current-station! []
  (reset! current-station nil))

(defn recalc-load-on-current-station! []
  (swap! current-station #(recalc-samples % recalc-load)))

;;--------------------------------------------------------------------------
;; Components 
;;--------------------------------------------------------------------------

(defn- right-arrow 
  ([]
   [:span {:dangerouslySetInnerHTML {:__html "&rarr;"}}])
  ([text]
   [:span text (right-arrow)]))

(defn- copyright 
  ([]
   [:span {:dangerouslySetInnerHTML {:__html "&copy;"}}])
  ([text]
   [:span (copyright) (str " ") text]))

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
     (str name)]
    [:nav.breadcrumb.card-header-icon
     {:aria-label "breadcrumbs"}
     [:ul 
      [:li {:key :recalc}
       [:a 
        {:on-click #(recalc-load-on-current-station!)}
        "Перерасчет"]]
      [:li {:key :reset}
       [:a
        {:on-click #(set-current-station! name)} 
        "Сброс"]]
      [:li {:key :close}
       [:a 
        {:on-click #(reset-current-station!)}
        "Закрыть"]]]]]
   [:div.card-content
    [:div.content.table-container
     [:table.table.is-striped.is-fullwidth
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

(defn StationTitle [name]
  [:div.card
   [:div.card-header
    [:p.card-header-title
     (str name)]
    [:a.card-header-icon 
     {:on-click #(set-current-station! name)}
     (right-arrow "Открыть")]]])

(defn StationItem [name]
  (let [current-name     (:name @current-station)
        current-sections (:sections @current-station)]
    (if (= name current-name)
      (Station name current-sections)
      (StationTitle name))))

;; (defn ErrorMsg [title msg]
;;   [:div.card
;;    [:div.card-header
;;     [:p.card-header-title title]]
;;    [:div.card-content
;;     [:div.content
;;      [:p msg]]]])

(defn ErrorMsg [title msg]
  [:div.message.is-danger.in-card
   [:div.message-header
    [:p title]]
   [:div.message-body
    [:p msg]]])

(defn Layout [& children]
  [:div.site
   [:nav.navbar.is-dark
    {:role "navigation"
     :aria-label "main navigation"}
    [:div.navbar-brand
     [:h1.navbar-item.title "Нагрузки 2019"]
     ]]
   [:section.section.site-content
    [:div.container
     [:div.columns.is-multiline
      (into [:div.column.is-10.is-offset-1] children)]]]
   [:footer.footer
    [:div.content {:style {:text-align "center"}}
     [:p 
      [:a {:href "https://github.com/lynch513/loads-web"}
       "https://github.com/lynch513/loads-web"]
      (str " ")
      (copyright [:span.is-italic "2019"])]]]])

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
      (ErrorMsg "Ошибка" error-msg))))

(extend-type e/SError
  IRenderLoads
  (render [this]
    (let [error-msg ((:key this) errors-map (:default-message errors-map))
          path      (:path this)
          path-msg  (string/join " -> " (remove string/blank? path))
          name      (first path)
          error-title (str (when name (str name " ")) "Ошибка")]
      (ErrorMsg error-title (str error-msg ": " path-msg)))))

;;--------------------------------------------------------------------------
;; Types and protocols extensions
;;--------------------------------------------------------------------------

(extend-type t/Station
  IRenderLoads
  (render [this]
    (let [{:keys [name]} this]
      (StationItem name))))

(extend-protocol IRenderLoads
  List
  LazySeq
  (render [this]
    (into [:<>] (for [item this] ^{:key item} (render item)))))

(extend-type nil
  IRenderLoads
  (render [this]
    nil))

;;--------------------------------------------------------------------------
;; Fetching data
;;--------------------------------------------------------------------------

(GET "stations.json" 
     {:response-format :json
      :keywords? true
      :handler (fn [arg]
                 (reset! station-list (map #(io/from-map ::t/->Station %) arg))
                 (.log js/console "Load data completed ..."))
      :error-handler (fn [_]
                       (reset! station-list (->IOError :error-on-data-loading))
                       (.log js/console (str "Error on data loading")))})

;;--------------------------------------------------------------------------
;; Main app 
;;--------------------------------------------------------------------------

(defn main-app []
  (Layout
    (render @station-list)))

(reagent/render-component [main-app]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

