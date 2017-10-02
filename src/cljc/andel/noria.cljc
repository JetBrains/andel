(ns andel.noria
  (:require [andel.render :as render]
            [andel.theme :as theme])
  (:import [andel.render LineInfo]))

(defn should-subtree-update [pred]
  (fn [r-f]
    (fn
      ([] {:s (r-f)
           :args nil})
      ([state & args]
       (if (or (nil? (:args state)) (pred args (:args state)))
         {:s (apply r-f (:s state) args)
          :args args}
         state))
      ([state] (r-f (:s state))))))

(defn stateless [render-fn]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state & args]
       (r-f state (apply render-fn args)))
      ([state] state))))

(defn pure []
  (should-subtree-update not=))

(def scroll
  (stateless
   (fn [{:keys [on-wheel child]}]
     [:div {:style (render/style {:display  "flex"
                                  :flex     "1"
                                  :overflow :hidden})
            :on-wheel on-wheel}
      child])))

(def render-line
  (comp
   (should-subtree-update
    (fn [[props metrics] [props' metrics']]
      (or (not= metrics metrics')
          (not (render/line-props-equiv? props props')))))
   (stateless
    (fn [props metrics]
      (let [^LineInfo line-info (render/build-line-info props)
            selection (.-selection line-info)
            caret (.-caret line-info)
            text (.-text line-info)
            fg-markup (.-foreground line-info)
            bg-markup (.-background line-info)]
        [:div {:class "render-line"}
         (when (some? selection)
           [:div {:key :selection
                  :style (render/style (render/selection-style selection metrics))}])
         [:raw-line {:key :line
                     :text text
                     :fg-markup fg-markup
                     :bg-markup bg-markup
                     :metrics metrics}]
         (when (some? caret)
           [:div {:key :active-line
                  :style (render/style (render/active-line-style metrics))}])
         (when (some? caret)
           [:div {:key :caret
                  :style (render/style (render/caret-style caret metrics))}])])))))

(def editor-viewport
  (comp
   (should-subtree-update
    (fn [state state']
      (not (identical? state state'))))
   (stateless
    (fn [state]
      (let [viewport (:viewport state)
            metrics (:metrics viewport)
            {:keys [y-shift] :as viewport-info} (render/viewport-info viewport)]
        (into
         [:div {:style (render/style {:background theme/background
                                      :width      "100%"
                                      :overflow   "hidden"})}]
         (map (fn [props]
                [:div {:key (:line-number props)
                       :style (render/style {:transform (str "translate3d(0px, " y-shift "px, 0px)")})}
                 [render-line props metrics]]))
         (render/viewport-lines state viewport-info)))))))

(def style-cmp
  (comp
   (should-subtree-update (constantly false))
   (stateless
    (fn [name style]
      [:style {:name name
               :style (render/style style)}]))))

(def styles-container
  (comp
   (should-subtree-update not=)
   (stateless
    (fn [s]
      (into
       [:div]
       (map (fn [[n s]] ^{:key n} [style-cmp n s]))
       s)))))

(def editor-component
  (comp
   (should-subtree-update
    (fn [[state callbacks] [state' callbacks']]
      (or (not (identical? state state'))
          (not= callbacks callbacks'))))
   (stateless
    (fn [state {:keys [on-input on-mouse-down on-drag-selection on-resize on-scroll on-focus on-key-down]} styles-map]
      [:div {:style (render/style {:display "flex"
                                   :flex    "1"
                                   :cursor  "text"
                                   :outline "transparent"})}
       [scroll
        {:on-wheel on-scroll
         :child [editor-viewport state]}]
       [styles-container styles-map]]))))

