(ns ai.ga
  (:require
   [clojure.core.match :only (match)]
   [clojure.math.numeric-tower :as math]
   [quil.core :as q]))

;;---------------------------------------------------------------
;; find opt path (traveling-salesman-problem slover)
;; from (J.J.Grefenstette, B.Gopal, B.J.Rosmaita, D.V.Gucht,
;; Genetic Algorithms for the Traveling Salesman Problem
;; Proceedings of the 1st International Conference on Genetic Algorithms, 1985)

;;----------------------------
;; Grefenstette encoding/decoding
(defn symbolic->grefnum
  "symbols->numbers(genetic code)"
  [points given-route]
  (loop [s [] c points p given-route]
    (if (empty? c) (reverse s)
        (let [target (first p)]
          (recur
           (cons (inc (.indexOf c target)) s)
           (remove #(= target %) c)
           (rest p))))))

(defn grefnum->symbolic
  "numbers(genetic code)->symbols"
  [points encoded-route]
  (loop [decoded-route [] c points e encoded-route]
    (if (empty? e) (reverse decoded-route)
        (let [target (nth c (dec (first e)))]
          (recur
           (cons target decoded-route)
           (remove #(= target %) c)
           (rest e))))))

;;----------------------------
;; selection
(defn get-winner [a b]
  (if (< (second a) (second b)) a b))

(defn tournament-selection [scored-population]
  (->> (partition 2 scored-population)
       (map #(first (get-winner (first %) (second %)))) ;; fetch winner
       (repeat 2) ;; copy
       (apply concat)))

(defn selection
  [scored-population]
  (tournament-selection scored-population))

;;----------------------------
;; crossover
(defn one-point
  [at genes]
  (let [a (split-at at (first genes))
        b (split-at at (second genes))]
    [(concat (first a) (second b))
     (concat (first b) (second a))]))

(defn crossover [population gene-size]
  (doall
   (->> (partition 2 population)
        (map (partial one-point (rand-int gene-size)))
        (reduce concat))))

;;----------------------------
;; mutation
(defn mutation
  [population gene-size]
  (letfn [(endamage
            [gene]
            (loop [i (dec gene-size) gene gene]
              (cond
               (= 0 i) gene
               (= 0 (rand-int 500)) ;; mutation (rate = 0.2%)
               (recur
                (dec i)
                (let [tail (drop (inc i) gene)]
                  (concat (take i gene) [(inc (rand-int (count tail)))] tail)))
               :else
               (recur (dec i) gene))))]
    (doall (map endamage population))))

;;----------------------------
;; find-opt-path
(defn mean-square [x1 x2 y1 y2]
  (math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))

(defn find-opt-path
  "最短巡回経路の近似解を求める。
  loop-limit: ループ上限
  pop-size: 1世代の上限"
  [points-with-coord loop-limit pop-size]
  (let [points (sort (keys points-with-coord))
        gene-size (count points)
        pop-size (if (odd? pop-size) (inc pop-size) pop-size)
        init-population
        (take pop-size (repeatedly #(->> points shuffle (symbolic->grefnum points))))]
    (letfn [(get-distance
              [point-A point-B]
              (let [a (get points-with-coord point-A)
                    b (get points-with-coord point-B)]
                (mean-square (first a) (first b) (second a) (second b))))
            (evaluate
              [route]
              (->> (cons (last route) (drop-last route))
                   (map get-distance route)
                   (reduce +)))
            (get-scored-routes
              [population]
              (map #(list %1 (evaluate (grefnum->symbolic points %1)))
                   population))]

      ;; main routine
      (loop [population init-population rest-loops loop-limit]
        (let [scored-routes (get-scored-routes (shuffle population))]
          (if (< rest-loops 1)
            ;; get result
            (->> (reduce get-winner scored-routes)
                 first
                 (grefnum->symbolic points))
            ;; selection -> mutation -> crossover repeatedly
            (recur
             (-> scored-routes
                 selection
                 (mutation gene-size)
                 (crossover gene-size))
             (dec rest-loops))))))))

;;---------------------------------------------------------------
;; graph plot

(def ^:const base-x "origin position" 30)

(def ^:const base-y "origin position" 30)

(def ^:const scale "scale size" 4)

(def ^:const circle-r "radius of circle" 13)

(def ^:const reversed-y "reverse for difference between Cartesian coordinate system and Gui System" 570)

(defn set-white
  "fill white"
  []
  (q/fill (q/color 255)))

(defn set-blue
  "fill blue"
  []
  (q/fill (q/color 0 102 153)))

(defn translate-position
  "translate position from Cartesian coordinate system to Gui System"
  [[x y]]
  [(+ (* scale x) base-x)
   (- reversed-y (+ (* scale y) base-y))])

(defn draw-arrow-head
  "draw arrow head '▶'"
  [xy1 xy2 top-mergin bot-mergin]
  (let [x1 (first xy1) x2 (first xy2)
        y1 (second xy1) y2 (second xy2)
        size (+ 25 (- bot-mergin))]
    (set-white)
    (q/push-matrix)
    (q/translate x1 y1)
    (q/rotate (+ (q/radians 270) (q/atan2 (- y2 y1) (- x2 x1))))
    (q/triangle 0 top-mergin 5 size -5 size)
    (q/pop-matrix)))

(defn draw-point-with-name
  "draw point circle with name"
  [name-keyword points]
  (let [xy (name-keyword points) x (first xy) y (second xy)]
    (set-white)
    (q/ellipse x y (* circle-r 2) (* circle-r 2))
    (set-blue)
    (q/text (name name-keyword) (- x 5) (+ y 5))))


(defn draw-cc-axis
  "draw axis of Cartesian coordinate system"
  [] 1)


(defn draw-route [points route]
  (let [points (zipmap (keys points) (map translate-position (vals points)))]
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
      (doall (map #(draw-point-with-name % points) (keys points)))

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
        (q/text "Y" (- (y-line 2) 5) (- (y-line 3) 5))))))

(defn plot-route [sample-points route]
  (q/defsketch skt1
    :title "routes"
    :setup (fn [] (q/frame-rate 30) (q/smooth)) ;; anti-aliased
    :draw (draw-route sample-points route)
    :size [600 600]))

;;---------------------------------------------------------------
;; execution example
;; (find-opt-path sample-points 100 50)
;; (plot-graph sample-points (find-opt-path sample-points-coord 200 200))

;;---------------------------------------------------------------
(defn get-euc-distance [[x y] [x' y']]
  (math/sqrt (+ (math/expt (- x x') 2) (math/expt (- y y') 2))))

(defn get-random-point [other-points min-distance max-x max-y]
  (letfn [(in-min-distance? [xy xy'] (< (get-euc-distance xy xy') min-distance ))
          (not-too-close? [xy] (empty? (filter #(in-min-distance? xy %) other-points)))]
    (first (filter not-too-close? (repeatedly (fn [] [(rand-int max-x) (rand-int max-y)]))))))

(defn generate-random-points [point-names min-distance max-x max-y]
  (letfn [(generate-points-with [[point-set points-with-name] name]
            (let [point (get-random-point point-set min-distance max-x max-y)]
              [(conj point-set point) (assoc points-with-name name point)]))]
    (second (reduce generate-points-with [{} {}] point-names))))

(defn do-inst []
  (let [points (ai.ga/generate-random-points [:a :b :c :d :e :f :g :h] 10 100 100)]
    (println points)
    (plot-route points (find-opt-path points 200 200))))
