(use '[clojure.core.match :only (match)]
     '[clojure.math.numeric-tower :as math])

;;---------------------------------------------------------------
(def sample-points-with-coord
  {:a [10 100] :b [30 20] :c [80 60] :d [60 100] :e [70 10]
   :f [5 100] :g [80 80] :h [30 30] :i [110 20] :j [0 120]})

;;---------------------------------------------------------------
;; find opt path

;;----------------------------
;; Grefenstette encoding/decoding
(defn symbolic->grefnum
  "grefenstetteらによる Symbols->Numbersの変換"
  [points given-route]
  (loop [s [] c points p given-route]
    (if (empty? c) (reverse s)
        (let [target (first p)]
          (recur
           (cons (inc (.indexOf c target)) s)
           (remove #(= target %) c)
           (rest p))))))

(defn grefnum->symbolic
  "symbolic->grefnumの逆変換 Numbers->Symbolsの変換"
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
  (prof
   :selection
   (doall
    (tournament-selection scored-population))))

;;----------------------------
;; crossover
(defn one-point
  [at genes]
  (let [a (split-at at (first genes))
        b (split-at at (second genes))]
    [(concat (first a) (second b))
     (concat (first b) (second a))]))

(defn crossover [population gene-size]
  (prof
   :crossover
   (doall
    (->> (partition 2 population)
         (map (partial one-point (rand-int gene-size)))
         (reduce concat)))))

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

(import (javax.swing JFrame))
(import (java.awt Color))
(import (java.awt Graphics))

(def radius 8)

(defn x-extend [x]
  (+ (* (- x radius) 3) 50))

(defn y-extend [y]
  (+ (- (* (- y radius) 3)) 400))

(defn plot-graph [points-with-coord route]
  (def frame (JFrame. "Route"))
  (doto frame
    (.setSize 450 450)
    (.setVisible true)
    (.setResizable false))
  (Thread/sleep 100) ;; wait for the generateion of Window

  (def graphics (.. frame (getGraphics)))

  (defn plot-point [name]
    (let [coord (name points-with-coord)
          r (* radius 2)
          x-pos (x-extend (first  coord))
          y-pos (y-extend (second coord))]
      (doto graphics
        (.setColor (Color. 255 100 100))
        (.fillOval x-pos y-pos r r)
        (.setColor (Color. 100 190 190))
        (.drawOval x-pos y-pos r r)
        (.setColor (Color. 0 0 0))
        (.drawString (str name) x-pos (+ y-pos 25)))))

  (defn draw-line [point-a point-b]
    (let [xy-a (point-a points-with-coord)
          xy-b (point-b points-with-coord)
          get-x #(+ radius (x-extend (first %1)))
          get-y #(+ radius (y-extend (second %1)))]
    (doto graphics
      (.setColor (Color. 0 0 0))
      (.drawLine (get-x xy-a) (get-y xy-a) (get-x xy-b) (get-y xy-b)))))

  ;; draw points
  (doall (map plot-point (keys points-with-coord)))

  ;; draw route
  (doall (map draw-line route (cons (last route) (drop-last route))))
  (println "done!"))

;;---------------------------------------------------------------
;; execution example
;; (find-opt-path sample-points 100 50)
;; (plot-graph sample-points (find-opt-path sample-points-coord 200 200))
(defn do-inst []
  (plot-graph sample-points-with-coord
              (find-opt-path sample-points-with-coord 200 200))
  (print-exec-time))
