(ns slurper.core
    (:require [slurper.lexer]
              [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to slurper"]
   [:div [:a {:href "/about"} "go to about page"]]])

(defn about-page []
  [:div [:h2 "About slurper"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app


(defn head []
  (aget (js/document.getElementsByTagName "head") 0))

(defn include-script [src cb]
  (let [e (js/document.createElement "script")]
    (aset e "onload" cb)
    (doto e
          (.setAttribute "type" "text/javascript")
          (.setAttribute "src" src))
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

(defonce *codemirror-state (atom :initial))

(defn with-codemirror [cb]
  (if (= @*codemirror-state :ready)
    (cb)
    (do
      (if (= @*codemirror-state :scheduled)
        nil
        (do
          (reset! *codemirror-state :scheduled)
          (include-script "/codemirror/addon/runmode/runmode-standalone.js"
                          (fn []
                            (include-script "/codemirror/mode/javascript/javascript.js"
                                            (fn [] (js/console.log "js load")))
                            (include-script "/codemirror/mode/clike/clike.js"
                                            (fn [] (js/console.log "clike load")))
                            (include-script "/codemirror/mode/clojure/clojure.js"
                                            (fn [] (js/console.log "clojure load")))
                            (cb))))))))

(defn mount-root []
  (with-virtualized
   #(with-codemirror
     (fn []
        (reagent/render [current-page] (.getElementById js/document "app"))))))

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
