(ns andel.throttling
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [chan >! <! timeout alts! put! close!]]))


(defn get-time! []
  (.now js/Date))


(defn throttle [f ms]
  "Call function on first event, then ignore events during ms"
  (let [*last-call-time (atom (get-time!))]
    (fn [& args]
      (if (>= (- (get-time!) @*last-call-time) ms)
          (let [res (apply f args)]
            (reset! *last-call-time (get-time!))
            res)
          nil))))


(defn debounce [in ms]
  (let [out (chan)]
    (go-loop [last-val nil]
      (let [val (if (nil? last-val) (<! in) last-val)
            timer (timeout ms)
            [new-val ch] (alts! [in timer])]
        (condp = ch
          timer (do (>! out val)
                    (recur nil))
          in (if new-val
                 (recur new-val)
                 (close! out)))))
    out))


(defn debounce-wrapper [f ms]
  "Call function on last event when nothing happens during ms"
  (let [in-args (chan)
        out-args (debounce in-args ms)]
    (go-loop [args (<! out-args)]
             (when args
                   (apply f args)
                   (recur (<! out-args))))
    (fn [& args]
      (put! in-args args)
      nil)))
