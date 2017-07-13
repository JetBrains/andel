(ns ^:figwheel-no-load andel.dev
  (:require [andel.core :as core]
            [figwheel.client :as figwheel :include-macros true]))

(enable-console-print!)

(core/init!)
