(ns andel.theme)

(def zenburn
  {:fg+1      "#FFFFEF"
   :fg        "#DCDCCC"
   :fg-1      "#656555"
   :bg-2      "#000000"
   :bg-1      "#2B2B2B"
   :bg-05     "#383838"
   :bg        "#3F3F3F"
   :bg+05     "#494949"
   :bg+1      "#4F4F4F"
   :bg+2      "#5F5F5F"
   :bg+3      "#6F6F6F"
   :red+1     "#DCA3A3"
   :red       "#CC9393"
   :red-1     "#BC8383"
   :red-2     "#AC7373"
   :red-3     "#9C6363"
   :red-4     "#8C5353"
   :orange    "#DFAF8F"
   :yellow    "#F0DFAF"
   :yellow-1  "#E0CF9F"
   :yellow-2  "#D0BF8F"
   :green-1   "#5F7F5F"
   :green     "#7F9F7F"
   :green+1   "#8FB28F"
   :green+2   "#9FC59F"
   :green+3   "#AFD8AF"
   :green+4   "#BFEBBF"
   :cyan      "#93E0E3"
   :blue+1    "#94BFF3"
   :blue      "#8CD0D3"
   :blue-1    "#7CB8BB"
   :blue-2    "#6CA0A3"
   :blue-3    "#5C888B"
   :blue-4    "#4C7073"
   :blue-5    "#366060"
   :magenta   "#DC8CC3"})

(def token-styles {:keyword {:color (zenburn :yellow)
                             :font-weight "500"}
                   :def {:color (zenburn :blue-1)}
                   :operator {:color (zenburn :cyan)}
                   :variable {:color (zenburn :fg)}

                   :variable-3 {:color (zenburn :blue+1)}
                   :meta {:color (zenburn :green+4)}
                   :number {:color (zenburn :blue-1)}
                   :atom {:color (zenburn :magenta)}
                   :comment {:color (zenburn :green)}
                   :string {:color (zenburn :red)}
                   :ws {}
                   :whatever {:color :yellow}
                   :selected {:color :white
                              :layer 100}})

(def background (zenburn :bg))
(def foreground (zenburn :fg))
(def selection (zenburn :bg-1))
