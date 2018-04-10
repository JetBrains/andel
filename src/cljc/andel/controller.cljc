(ns andel.controller
  (:require [andel.utils :as utils]
            [andel.text :as text]
            [andel.cursor :as cursor]
            [andel.intervals :as intervals]
            [andel.core :as core]))

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

(defn set-caret-at-offset [{:keys [document] :as state} caret-offset' selection?]
  (let [text (:text document)
        caret (-> state :editor :caret)]
    (-> state
        (cond-> selection?
          (update-in [:editor :selection] update-selection caret {:offset caret-offset' :v-col 0}))
        (cond-> (not selection?)
          (assoc-in [:editor :selection] [caret-offset' caret-offset']))
        (assoc-in [:editor :caret :offset] caret-offset')
        (assoc-in [:editor :caret :v-col] (:col (utils/offset->line-col caret-offset' text))))))

(defn- move-cursor-at-line-start [cursor]
  (let [[cursor' end?] (cursor/move-while cursor #(not= % \newline) :backward)]
    (if end?
      cursor'
      (cursor/next cursor'))))

(defn home [{:keys [document] :as state} selection?]
  (let [{:keys [text lexer]} document
        caret-offset (max 0 (dec (core/caret-offset state)))
        cursor (cursor/make-cursor text caret-offset)
        line-start-cursor (move-cursor-at-line-start cursor)
        text-start-cursor (first (cursor/move-while line-start-cursor
                                                    #(or (= % \space)
                                                         (= % \tab)) :forward))
        line-start-offset (cursor/offset line-start-cursor)
        text-start-offset (cursor/offset text-start-cursor)]
    (if (and (< line-start-offset caret-offset)
             (< caret-offset text-start-offset))
      (set-caret-at-offset state line-start-offset selection?)
      (set-caret-at-offset state text-start-offset selection?))))

(defn end [{{:keys [caret]} :editor
            {:keys [text]} :document
            :as state} selection?]
  (let [carret-line (get-caret-line caret text)]
    (set-caret-at-line-end state (get-caret-line caret text) selection?)))

(def whitespace? #{\newline \space \tab})

(defn next-word-delta [state]
  (let [text (-> state :document :text)
        text-len (text/text-length text)
        caret-offset (core/caret-offset state)]
    (if (< caret-offset text-len)
      (let [cursor (cursor/make-cursor text caret-offset)
            word-begin-cursor (first (cursor/move-while cursor whitespace? :forward))
            [word-end-cursor end-of-text?] (cursor/move-while word-begin-cursor (complement whitespace?) :forward)
            delta (cursor/distance cursor word-end-cursor)]
        (cond-> delta end-of-text? inc))
      0)))

(defn prev-word-delta [state]
  (let [text (-> state :document :text)
        text-len (text/text-length text)
        caret-offset (core/caret-offset state)]
    (if (< 0 caret-offset)
      (let [cursor (cursor/prev (cursor/make-cursor text caret-offset))
            word-end-cursor (first (cursor/move-while cursor whitespace? :backward))
            [word-begin-cursor start-of-text?] (cursor/move-while word-end-cursor (complement whitespace?) :backward)
            delta (- (cursor/distance cursor word-begin-cursor))]
        (cond-> delta start-of-text? dec))
      0)))

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

(defn put-to-clipboard [state content]
  (-> state
      (assoc-in [:editor :clipboard :content] content)
      (update-in [:editor :clipboard :timestamp] inc)))

(defn conj-to-clipboard [state content]
  (-> state
      (update-in [:editor :clipboard :content] str content)
      (update-in [:editor :clipboard :timestamp] inc)))

(defn copy [state]
  (let [text (get-in state [:document :text])
        [sel-from _ :as selection] (core/selection state)
        sel-len (selection-length selection)
        selected-text (str (core/text-at-offset text sel-from sel-len))]
    (cond-> state
      (< 0 sel-len)
      (put-to-clipboard selected-text))))

(defn cut [state]
  (let [text (get-in state [:document :text])
        [sel-from _ :as selection] (core/selection state)
        sel-len (selection-length selection)
        selected-text (str (core/text-at-offset text sel-from sel-len))]
    (cond-> state
      (< 0 sel-len)
      (-> (core/delete-at-offset sel-from sel-len)
          (put-to-clipboard selected-text)))))

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
