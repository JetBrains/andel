(ns andel.controller
  (:require [andel.utils :as utils]
            [andel.text :as text]
            [andel.intervals :as intervals]
            [andel.core :as core]
            [andel.parens :as parens]))

(defn selection-length [[left right :as selection]]
  (assert (<= left right) (str "Wrong selection positioning: " selection))
  (- right left))

(defn drop-virtual-position [caret text]
  (let [{:keys [line col]} (utils/offset->line-col (:offset caret) text)]
    (assoc caret :v-col col)))

(defn backspace [state]
  (let [selection (core/selection state)
        sel-from  (nth selection 0)
        sel-length (selection-length selection)
        caret-offset (core/caret-offset state)]
    (as-> state st
          (cond
            (< 0 sel-length) (core/delete-at-offset st sel-from sel-length)
            (= 0 caret-offset) st
            :else (core/delete-at-offset st (dec caret-offset) 1))
          (core/move-view-if-needed st)
          (update-in st [:editor :caret] drop-virtual-position (get-in st [:document :text])))))

(defn delete [state]
  (let [selection (core/selection state)
        text-len (text/text-length (-> state :document :text))
        sel-len (selection-length selection)
        sel-from (nth selection 0)
        caret-offset (core/caret-offset state)]
    (as-> state st
          (cond
            (< 0 sel-len) (core/delete-at-offset st sel-from sel-len)
            (<= text-len caret-offset) st
            :else (core/delete-at-offset st caret-offset 1))
          (core/move-view-if-needed st)
          (update-in st [:editor :caret] drop-virtual-position (get-in st [:document :text])))))

(defn type-in [{:keys [editor] :as state} str]
  (let [str-len (count str)
        caret-offset (core/caret-offset state)
        selection (core/selection state)
        selection-len (selection-length selection)]
    (-> state
        (cond-> (< 0 selection-len)
                (core/delete-at-offset (first selection) selection-len))
        (core/insert-at-offset caret-offset str)
        (core/move-view-if-needed)
        (as-> st (update-in st [:editor :caret] drop-virtual-position (get-in st [:document :text]))))))

(defn update-selection [[from to :as selection] old-caret new-caret]
  (let [caret-offset  (core/caret->offset old-caret)
        caret-offset' (core/caret->offset new-caret)]
    (cond
      (= caret-offset from)
      [(min caret-offset' to) (max caret-offset' to)]

      (= caret-offset to)
      [(min from caret-offset') (max from caret-offset')]

      :else
      [(min caret-offset caret-offset') (max caret-offset' caret-offset')])))

(defn drop-selection-on-esc [state]
  (let [caret-offset (core/caret-offset state)]
    (core/set-selection state [caret-offset caret-offset] caret-offset)))

(defn restrict-to-text-length [offset text]
  (let [text-length (text/text-length text)]
    (-> offset (max 0) (min text-length))))

(defn translate-caret [caret text delta-offset]
  (assoc caret :offset (-> (:offset caret)
                           (+ delta-offset)
                           (restrict-to-text-length text))))

(defn translate-caret-verticaly [{v-col :v-col :as caret} text delta-line]
  (let [carret-offset (core/caret->offset caret)
        {:keys [line col]} (utils/offset->line-col carret-offset text)
        to-line (+ line delta-line)
        line-len (utils/line-length to-line text)
        new-v-col (if (some? v-col)  (max v-col col) col)
        new-col (min line-len new-v-col)]
    {:offset (utils/grid-pos->offset {:line to-line :col new-col} text)
     :v-col new-v-col}))

(defn get-caret-line [caret text]
  (let [{caret-offset :offset} caret
        line (utils/offset->line caret-offset text)]
    line))

(defn get-line-ident [text line]
  (let [loc (text/scan-to-line-start (text/zipper text) line)
        line-text (text/text loc (text/distance-to-EOL loc))
        trimmed (clojure.string/triml line-text)
        ident-size (- (count line-text) (count trimmed))]
    (subs line-text 0 ident-size)))

(defn on-enter [{:keys [editor document] :as state}]
  (let [text (:text document)
        line (get-caret-line (:caret editor) text)
        identation (get-line-ident text line)]
    (type-in state (str "\n" identation))))

(defn set-caret-at-grid-pos [{:keys [editor document] :as state} grid-pos selection?]
  (let [{:keys [caret selection]} editor
        {:keys [text]} document
        caret-offset' (utils/grid-pos->offset grid-pos text)
        caret'   (-> {:offset caret-offset'
                      :v-col 0}
                     (drop-virtual-position text))
        selection' (if selection?
                     (update-selection selection caret caret')
                     [caret-offset' caret-offset'])]
    (-> state
        (assoc-in [:editor :caret] caret')
        (assoc-in [:editor :selection] selection'))))

(defn set-caret-at-line-begining
  [state line selection?]
  (set-caret-at-grid-pos state {:line line :col 0} selection?))

(defn set-caret-at-line-end
  [state line selection?]
  (set-caret-at-grid-pos state {:line line :col #?(:cljs js/Number.POSITIVE_INFINITY
                                                   :clj Integer/MAX_VALUE)} selection?))

(defn pg-move [{:keys [document viewport] :as state} dir selection?]
  (let [{:keys [text]} document
        {:keys [metrics]} viewport
        view-size-in-lines (core/count-lines-in-view viewport metrics)
        sign (case dir :up - :down +)]
    (-> state
        (update-in [:editor :caret] translate-caret-verticaly text (sign view-size-in-lines))
        (core/move-view-if-needed))))

(defn home [{{:keys [caret]} :editor
             {:keys [text]} :document
             :as state} selection?]
  (let [carret-line (get-caret-line caret text)]
    (set-caret-at-line-begining state (get-caret-line caret text) selection?)))

(defn end [{{:keys [caret]} :editor
            {:keys [text]} :document
            :as state} selection?]
  (let [carret-line (get-caret-line caret text)]
    (set-caret-at-line-end state (get-caret-line caret text) selection?)))

(defn next-word-delta [state]
  (let [text (-> state :document :text)
        caret-offset (core/caret-offset state)
        {caret-line :line caret-col :col} (utils/offset->line-col caret-offset text)
        line-text (text/line-text text caret-line)]
    (max (some-> (re-find #"^.+?\b" (subs line-text caret-col))
                          count)
                  1)))

(defn prev-word-delta [state]
  (let [text (-> state :document :text)
        caret-offset (core/caret-offset state)
        {caret-line :line caret-col :col} (utils/offset->line-col caret-offset text)
        line-text (text/line-text text caret-line)]
    (min (- (some-> (second (re-find #".*(\b.+)$" (subs line-text 0 caret-col)))
                    count))
        -1)))

(defn move-caret [{:keys [document editor] :as state} dir selection?]
  (let [{:keys [caret selection]} editor
        text (:text document)
        caret'     (case dir
                     :left  (-> caret
                                (translate-caret text -1)
                                (drop-virtual-position text))
                     :right (-> caret
                                (translate-caret text 1)
                                (drop-virtual-position text))
                     :word-forward (-> caret
                                       (translate-caret text (next-word-delta state))
                                       (drop-virtual-position text))
                     :word-backward (-> caret
                                        (translate-caret text (prev-word-delta state))
                                        (drop-virtual-position text))
                     :up    (translate-caret-verticaly caret text -1)
                     :down  (translate-caret-verticaly caret text 1))
        caret-offset' (core/caret->offset caret')
        selection' (if selection?
                     (update-selection selection caret caret')
                     [caret-offset' caret-offset'])]
    (-> state
        (assoc-in [:editor :caret] caret')
        (assoc-in [:editor :selection] selection')
        (core/move-view-if-needed))))


(defn scroll [{:keys [document viewport] :as state} {:keys [x y width height]}]
  (let [;screen-height (get-in viewport [:view-size 1])
        line-height (utils/line-height (:metrics viewport))
        lines-count (text/lines-count (:text document))
        document-height (* lines-count line-height)
        ;allowed-y-offset (max 0 (- document-height (/ screen-height 2)))
        abs (fn [x] (max x (- x)))]
    (update state :viewport merge {:pos [x y]
                                   :reason :scroll
                                   :view-size [width height]})
    #_(update-in state [:viewport :pos]
               (fn [[x y]]
                 (if (< (abs dx) (abs dy))
                   [x (min allowed-y-offset (max 0 (+ y dy)))]
                   [(max 0 (+ x dx)) y])))))

(defn resize [state width height]
  (assoc-in state [:viewport :view-size] [width height]))

(let [a (atom 0)]
  (def unique-paren-id #(swap! a inc)))

(defn highlight-parens [{:keys [document] :as state}]
  (let [caret-offset  (core/caret-offset state)
        lexer (:lexer document)
        paren-offsets (parens/find-parens (:text document)
                                          (if (some? lexer)
                                            #(intervals/is-brace-token? lexer %)
                                            (constantly true))
                                          caret-offset)
        old-paren-ids (:paren-ids document)]
    (-> state
        (core/delete-markers old-paren-ids)
        ((fn [state]
           (or (when-let [[p-from p-to] paren-offsets]
                 (when (and p-from p-to)
                   (let [from-id (str "paren-" (unique-paren-id))
                         to-id   (str "paren-" (unique-paren-id))]
                     (-> state
                         (core/insert-markers [(intervals/->Marker p-from
                                                                   (inc p-from)
                                                                   false
                                                                   false
                                                                   (intervals/->Attrs from-id "highlight-paren" "" :background))
                                               (intervals/->Marker p-to
                                                                   (inc p-to)
                                                                   false
                                                                   false
                                                                   (intervals/->Attrs to-id "highlight-paren" "" :background))])
                         (assoc-in [:document :paren-ids] [from-id to-id])))))
               state))))))
