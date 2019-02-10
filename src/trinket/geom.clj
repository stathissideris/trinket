(ns trinket.geom
  (:require [trinket.ui :as ui]))


(defn fully-within?
  "Checks that the rectangle of the first argument is fully within the
  rectangle of the second argument."
  [{x1 ::ui/ax
    y1 ::ui/ay
    w1 ::ui/w
    h1 ::ui/x}
   {x2 ::ui/ax
    y2 ::ui/ay
    w2 ::ui/w
    h2 ::ui/x}]
  (let [left1   (+ x1 w1)
        bottom1 (+ y1 h1)
        left2   (+ x2 w2)
        bottom2 (+ y2 h2)]
    (and (>= x1 x2)
         (>= y1 y2)
         (<= left1 left2)
         (<= bottom1 bottom2))))

(defn offset-correction [{ox  ::ui/ax
                          oy  ::ui/ay
                          ow  ::ui/w
                          oh  ::ui/x
                          :as object}
                         {vx  ::ui/ax
                          vy  ::ui/ay
                          vw  ::ui/w
                          vh  ::ui/x
                          :as viewport}]
  (cond (fully-within? object viewport)
        {:dx 0 :dy 0}

        (> ow vw)
        {:dx 0 :dy 0}

        (> oh vh)
        {:dx 0 :dy 0}

        :else
        (let [v-left   (+ vx vw)
              v-bottom (+ vy vh)
              o-left   (+ ox ow)
              o-bottom (+ oy oh)]

          {:dx (cond (< ox vx)
                     (- vx ox)

                     (< v-left o-left)
                     (- o-left v-left)

                     :else 0)

           :dy (cond (< oy vy)
                     (- vy oy)

                     (< v-bottom o-bottom)
                     (- o-bottom v-bottom)

                     :else 0)})))
