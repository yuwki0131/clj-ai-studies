(ns ai.neuralnetwork
  (:require
   [incanter.core :as in]
   [incanter.stats :as st]
   [incanter.charts :as ch]))

(defn inner-product
  "xsとysの内積"
  [xs ys]
  (reduce + (map * xs ys)))

(defn norm
  "xsのユークリッドノルム"
  [xs]
  (in/sqrt (reduce + (map #(in/pow % 2) xs))))

(defn loop-to-fix
  "不動点に達するまでループ(学習)
  学習後のパラメータを返す。"
  [label+xss]
  (let [limit 10000 ;; limit of loop (when faild to learn)
        roh 0.2 ;; learning coefficient
        r (apply max (map (comp norm second) label+xss))] ;; R = max_i (|[xs_i]|)
    (letfn [(modify [[modified? ws bias] [label xs]]
              (if (< (* label (+ (inner-product ws xs) bias)) 0)
                [true ;; update flag
                 (map #(+ %1 (* roh label %2)) ws xs)
                 (+ bias (* roh label r))]
                ;; 学習後、パラメータに変化があったかどうか
                [modified? ws bias]))]
      (loop [ws [0 0] bias 0.001 i 0]
        (let [[modified? ws bias] (reduce modify [false ws bias] label+xss)]
          (if (or (not modified?) (< limit i))
            [ws bias]
            (recur ws bias (inc i))))))))

(defn plot-clusters-with-function
  "関数とクラスタのプロット
   f: 関数
   clusters: クラスタ群"
  [f [fcluster & rclusters :as clusters]]
  (let [init-plot (ch/scatter-plot (map first fcluster) (map second fcluster))
        all-x-elements (map first (apply concat clusters))
        x1 (apply min all-x-elements)
        x2 (apply max all-x-elements)]
    (reduce #(ch/add-points %1 (map first %2) (map second %2))
            (ch/add-function init-plot f x1 x2)
            rclusters)))

(defn display-result
  "学習後の関数とクラスタを表示
  bias: 学習後のパラメータ
  two-clusters:学習対象の各クラスタ

  学習後のパラメータは以下の関数として表示。
  ws = [a b], xs = (x, y) (or = (x_1, x_2))
  g(xs) = (ws . xs) + bias = ax + by + bias = 0
  y = - (ax + bias) / b"
  [[[a b] bias] two-clusters]
  (plot-clusters-with-function
   (fn [x] (- (/ (+ (* a x) bias) b)))
   two-clusters))

(defn merge-clusters-with-label
  "ラベルを付けて2つのクラスタをマージ"
  [[cluster-a cluster-b]]
  (concat (map #(vector -1 %) cluster-a)
          (map #(vector 1 %) cluster-b)))

(defn sperceptron
  "単純パーセプトロン。
  incanterで学習対象の各クラスタと学習後の直線を表示。学習後のパラメータを返す。
  two-clusters: クラスタを2つ指定"
  [two-clusters]
  (let [label+xss (merge-clusters-with-label two-clusters)
        ;; 学習後のパラメータ
        ws+bias (loop-to-fix label+xss)]
    (in/view (display-result ws+bias two-clusters))
    ws+bias))

(defn generate-sample-cluster
  "サンプルクラスタ生成(ガウス分布による)
   x, y: クラスタの中心点
   sd: 標準偏差(standard deviation)
   n: 要素数"
  [x y sd n]
  (let [generate-by #(st/sample-normal n :mean (+ % (rand 2)) :sd sd)]
    (map vector (generate-by x) (generate-by y))))

;; 以下のパラメータを弄る
;; (ai.neuralnetwork/sperceptron [(generate-sample-cluster 1 0 2 100)
;;                                (generate-sample-cluster 10 9 2 100)])

;; 学習に成功する例
;; (ai.neuralnetwork/sperceptron [(generate-sample-cluster -4 -2 2 100)
;;                                (generate-sample-cluster 10 9 2 100)])

;; 学習に失敗する例
;; (ai.neuralnetwork/sperceptron [(generate-sample-cluster 4 2 2 100)
;;                                (generate-sample-cluster 8 7 2 100)])

;; 参考
;; 学習後の関数: f(xs) = (ws . xs) + bias
;; ws: 各パラメータの重み
;; bias: 重み(上記以外)
