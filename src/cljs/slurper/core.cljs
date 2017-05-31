(ns slurper.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant])
    (:require-macros [reagent.interop :refer [$ $!]]))

;; -------------------------
;; Views

(def state (reagent/atom {:lines (vec (take 500 (repeat "hello world")))}))

@state

(defn editor [state]
  [:> js/ReactVirtualized.AutoSizer
   (fn [m]
     (reagent/as-element
      [:> js/ReactVirtualized.List
       {:height ($ m :height)
        :width ($ m :width)
        :style {:font-family "Fira Code"}
        :rowCount (count (:lines @state))
        :rowHeight 17
        :rowRenderer (fn [s]
                       (let [;{:keys [index style isVisible isScrolling]} (js->clj s)
                             index ($ s :index)
                             style ($ s :style)]
                         (reagent/as-element ^{:key index} [:div {:style style} (nth (:lines @state) index)])))
        :noRowsRenderer (fn []
                          (reagent/as-element [:div "hello empty"]))}]))])

(defn main []
  [:div {:style {:display :flex
                 :flex "1"}}
   [editor state]])

(defn head []
  (aget (js/document.getElementsByTagName "head") 0))

(defn include-script [src cb]
  (let [e (js/document.createElement "script")]
    (doto e
      (.setAttribute "type" "text/javascript")
      (.setAttribute "src" src))
    (aset e "onload" cb)
    (.appendChild (head) e)))

(defonce *virtualized-state (atom :initial))

(defn with-virtualized [cb]
  (if (= @*virtualized-state :ready)
    (cb)
    (do
      (if (= @*virtualized-state :scheduled)
        nil
        (do
          (reset! *virtualized-state :scheduled)
          (include-script "/react-virtualized.js" 
                          (fn []
                            (reset! *virtualized-state :ready)
                            (cb))))))))

(defn mount-root []
  (with-virtualized 
    #(reagent/render [main] (.getElementById js/document "app"))))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
