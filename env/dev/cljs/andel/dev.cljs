(ns ^:figwheel-no-load slurper.dev
  (:require [slurper.core :as core]
            [figwheel.client :as figwheel :include-macros true]))

(enable-console-print!)

(core/init!)
