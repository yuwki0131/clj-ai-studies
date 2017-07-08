;; an example of plotting route in XY-coord with Clojure/quil
(ns plot-route
  (:require [quil.core :as q]))

(def sample-points
  {:a [20 70] :b [30 20] :c [50 50] :d [60 59] :e [40 30]
   :f [20 50] :g [80 60] :h [10 30] :i [65 10] :j [40 80]
   :k [10 90] :l [70 90] :m [90 90] :n [70 75] :o [35 44]})

(def route [:a :f :j :d :g :i :c :e :b :h])

(defn draw-route [points route]
  (let [base-x 30 base-y 30
        reversed-y 570 mag 5.5 circle-r 13
        set-white #(q/fill (q/color 255))
        set-blue #(q/fill (q/color 0 102 153))
        trans #(list (+ (* mag (first %)) base-x) ;; to fix to real xy-axis
                     (- reversed-y (+ (* mag (second %)) base-y)))
        points (zipmap (keys points) (map trans (vals points)))]
    (letfn [(drop-colon [point]
              (apply str (rest (vec (str point)))))
            (draw-arrow-head [xy1 xy2 top-mergin bot-mergin]
              (let [x1 (first xy1) x2 (first xy2)
                    y1 (second xy1) y2 (second xy2)
                    size (+ 25 (- bot-mergin))]
                (set-white)
                (q/push-matrix)
                (q/translate x1 y1)
                (q/rotate (+ (q/radians 270) (q/atan2 (- y2 y1) (- x2 x1))))
                (q/triangle 0 top-mergin 5 size -5 size)
                (q/pop-matrix)))
            (draw-point-with-name [name]
              (let [xy (name points) x (first xy) y (second xy)]
                (set-white)
                (q/ellipse x y (* circle-r 2) (* circle-r 2))
                (set-blue)
                (q/text (drop-colon name) (- x 5) (+ y 5))))]
      (fn []
        ;; points & lines
        (q/background 255) ;; white background
        (q/stroke-weight 1)
        ;; draw lines
        (doall (map #(q/line (%1 points) (%2 points))
                    route (rest route)))
        (doall (map #(draw-arrow-head (%2 points) (%1 points) circle-r 0)
                    route (rest route)))
        ;; draw points with name
        (set-blue)
        ;; if this font is not in the environment, this code is ignored
        (q/text-font (q/create-font "DejaVu Sans Mono Bold" 16 true))
        (doall (map draw-point-with-name (keys points)))

        ;; XY-axis
        (q/stroke-weight 2)
        (let [x0 base-x, y0 (- reversed-y base-y)
              mergin 20, font-size 20, length 430
              x-line [(- x0 mergin) y0 (+ x0 length) y0]
              y-line [x0 (+ y0 mergin) x0 (- y0 length)]]
          (apply q/line x-line)
          (apply q/line y-line)
          (draw-arrow-head (drop 2 x-line) (take 2 x-line) 0 10)
          (draw-arrow-head (drop 2 y-line) (take 2 y-line) 0 10)
          (set-blue)
          (q/text-size font-size)
          (q/text "O" (+ 20 x0) (+ 20 y0))
          (q/text "X" (+ 5 (x-line 2)) (+ (x-line 3) 7)) ;; suitably
          (q/text "Y" (- (y-line 2) 5) (- (y-line 3) 5)))))))

(defn plot-route [sample-points route]
  (q/defsketch skt1
    :title "routes"
    :setup (fn [] (q/frame-rate 30) (q/smooth)) ;; anti-aliased
    :draw (draw-route sample-points route)
    :size [600 600]))
