(ns slurper.handler
  (:require [compojure.core :refer [GET defroutes]]
            [compojure.route :refer [not-found resources]]
            [hiccup.page :refer [include-js include-css html5]]
            [slurper.middleware :refer [wrap-middleware]]
            [config.core :refer [env]]))

(def mount-target
  [:div#app {:style "width:100%;height:100%;display:flex;"}
      [:h3 "ClojureScript has not been compiled!"]
      [:p "please run "
       [:b "lein figwheel"]
       " in order to start the compiler"]])

(defn head []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   #_[:link {:href "/firacode/fira_code.css"
           :rel "stylesheet"
           :type "text/css"
             :async "false"}]
   (include-css "/react-virtualized.css")])

(defn loading-page []
  (html5
    (head)
    [:body {:style "height: 100vh; margin-left: 0; margin-top: 0; overflow: hidden"}
     mount-target
     (include-js "/js/app.js")]))


(defroutes routes
  (GET "/" [] (loading-page))
  (GET "/about" [] (loading-page))
  
  (resources "/")
  (not-found "Not Found"))

(def app (wrap-middleware #'routes))
