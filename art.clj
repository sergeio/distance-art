(ns art
  (:import (javax.swing JFrame JLabel)
           (java.awt Graphics Dimension Color)
           (java.awt.image BufferedImage)))

(defn draw-circle [gfx x y radius]
  (.drawOval gfx (- x radius) (- y radius) (* 2 radius) (* 2 radius)))

(defn paint-canvas [buffer size graphics]
  (doseq [x (range size)
          y (range size)]
    (.setColor graphics (get-in buffer [x y]))
    (.drawLine graphics x y x y)))

(defn draw [buffer size]
  (let [image  (BufferedImage. size size BufferedImage/TYPE_INT_RGB)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g image 0 0 this)))]

    (paint-canvas buffer size (.createGraphics image))

    (doto (JFrame.)
      (.add canvas)
      (.setSize (Dimension. size size))
      (.show))))

(defn calculate-colors [size closest-point-fn]
  (into [] (for [x (range size)]
             (into [] (for [y (range size)]
                        (let [[_ _ r g b] (closest-point-fn x y)]
                          (Color. r g b)))))))


;;;;;;;;;;;;;;;
;util functions
;;;;;;;;;;;;;;;

(defn random-points [num-points x y]
  "Create random [x y r g b] points."
  (for [_ (range num-points)]
    [(rand-int x) (rand-int y) (rand-int 256) (rand-int 256) (rand-int 256)]))

(defn divisible-by-fn [by-what]
  "Curry function that will test if a number is divisible by-something."
  (fn [number] (let [quotient (/ (int (mod number 1000000)) by-what)]
                 (= (int quotient) quotient))))

(defn square [x] (* x x))

(defn min-by [f coll]
  "Returns the (min (map f coll))"
  (when (seq coll)
    (reduce (fn [min this] (if (< (f min) (f this)) min this))
            coll)))

(defn abs [x]
  (if (>= x 0) x (- x)))

(defn average [nums]
  (/ (reduce + nums) (count nums)))

(defn prinret [value]
  (println value)
  value)


;;;;;;;;;;;;;;;;;;;;;;;;;
; distance transformators
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compose [f & fs]
  (if (empty? fs) f
    (let [second-func-curried (partial (first fs) f)]
      (apply compose (cons second-func-curried (rest fs))))))

(defn get-closest-point-fn [f-distance set-points]
  "Turns a distance function into a get-closest-point function."
  (fn [x1 y1]
    (let [distance-to-set-point (fn [[x2 y2 _ _ _]] (f-distance x1 y1 x2 y2))]
      (min-by distance-to-set-point set-points))))

(defn noise-blur [f x1 y1 x2 y2]
  "Takes any distance function and essentially adds noise"
  (if (zero? (rand-int 2))
    (* 2 (f x1 y1 x2 y2))
    (f x1 y1 x2 y2)))


;;;;;;;;;;;;;;;;;;;
;distance functions
;;;;;;;;;;;;;;;;;;;

(defn d-avg [x1 y1 x2 y2]
  "Distance that averages the distance in the x direction and the y direction"
  (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2))

(defn d-avg-rand-neg [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (zero? (rand-int 2)) (- average) average)))

(defn d-avg-rand-dbl [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (zero? (rand-int 2)) (* 2 average) average)))

(defn d-linear [x1 y1 x2 y2]
  "Sum of the distance in the x direction and the y"
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn d-linear-r [x1 y1 x2 y2]
  "-1 * Sum of the distance in the x direction and the y"
  (- (+ (abs (- x2 x1)) (abs (- y2 y1)))))

(defn d-euclidean [x1 y1 x2 y2]
  "Euclidean Distance"
  (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))

(defn d-euclidean-r [x1 y1 x2 y2]
  "Negative euclidean distance"
  (- (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

(defn d-max [x1 y1 x2 y2]
  "Distance that is the maximum of the distance in the x direction, and the y"
  (let [xdist (abs (- x2 x1)) ydist (abs (- y2 y1))]
    (max xdist ydist)))

(defn d-min [x1 y1 x2 y2]
  "Distance that is the minimum of the distance in the x direction, and the y"
  (let [xdist (abs (- x2 x1)) ydist (abs (- y2 y1))]
    (min xdist ydist)))

(defn d-min-max [x1 y1 x2 y2]
  "Distance that averages min-dist and max-dist."
  (let [min-dist (d-min x1 y1 x2 y2)
        max-dist (d-max x1 y1 x2 y2)]
    (/ (+ min-dist max-dist) 2)))

(defn scanline [f-shape & {:keys [width] :or {width 2}}]
  "Make scanline function using f-shape for shape."
  (fn [f-dist x1 y1 x2 y2]
    (if ((divisible-by-fn width) (f-shape x1 y1 x2 y2))
      Double/MAX_VALUE
      (f-dist x1 y1 x2 y2))))

(defn scanline2 [f-shape & {:keys [width] :or {width 2}}]
  "Make scanline function using f-shape for shape."
  (fn [f-dist x1 y1 x2 y2]
    (if ((divisible-by-fn width) (f-shape x1 y1 x2 y2))
      (f-shape x1 y1 x2 y2)
      (f-dist x1 y1 x2 y2))))


; some example composite "distance" functions:
(def busy (compose d-min (scanline d-euclidean)))
(def scanny (compose d-max (scanline d-euclidean)))
(def noisy (compose d-min (scanline + :width 5) noise-blur))
(def gcp1 (compose d-min (scanline d-min :width 5)))
(def gcp2 (compose d-max (scanline d-linear-r)))
(def gcp3 (compose d-max (scanline d-euclidean-r :width 4)))
(def gcp4 (compose d-max (scanline d-euclidean-r)))
(def gcp5 (compose d-max (scanline d-min :width 3)))
(def gcp6 (compose d-max (scanline d-euclidean)))
(def gcp7 (compose d-max (scanline d-avg-rand-dbl)))
(def t1 (compose d-min (scanline + :width 5) noise-blur))
(def t2 (compose d-min noise-blur (scanline + :width 5)))


;;;;;;;;;;;;;;;;;
; Post-processing
;;;;;;;;;;;;;;;;;

(defn neighbors [x y radius]
  "Get the [x y] coordinates of points within a radius.  Can be negative."
  (for [i (range (- radius) (inc radius))
        j (range (- radius) (inc radius))]
    [(+ x i) (+ y j)]))

(defn neighbors-colors [grid x y radius]
  "Get the Colors of all the points in the grid within a radius of (x, y)."
  (for [[neighbors-x neighbors-y] (neighbors x y radius)]
    (get-in grid [neighbors-x neighbors-y])))

(defn split-into-rgb [color]
  "Takes a java.awt.Color, and splits into component [r g b]."
  [(.getRed color) (.getGreen color) (.getBlue color)])

(defn edgy-colors [colors]
  "Combines the java.awt.Colors in a weird way. Yes. It's very technical."
  (->> colors
       (#(remove nil? %))
       (map #(.getRGB %))
       average Color.))

(defn average-rgb-vectors [rgb-vectors]
  "Takes a sequence of [r g b] vectors, and returns the average [r g b]."
  [(average (map first rgb-vectors))
   (average (map second rgb-vectors))
   (average (map #(nth % 2) rgb-vectors))])

(defn average-colors [colors]
  "Takes in a sequence of java.awt.Color, and averages them into 1 Color"
  (->> colors
       (#(remove nil? %))
       (map #(split-into-rgb %))
       average-rgb-vectors
       (#(map int %))
       ((fn [[r g b]] (Color. r g b)))))

(defn pp-edges [grid & {:keys [radius] :or {radius 2}}]
  "Post processing of grid: make the edges weirder."
  (into [] (for [x (range (.length grid))]
   (into [] (for [y (range (.length (first grid)))]
              (edgy-colors (neighbors-colors grid x y radius)))))))

(defn pp-blur [grid & {:keys [radius] :or {radius 2}}]
  "Post processing of grid: blur"
  (into [] (for [x (range (.length grid))]
   (into [] (for [y (range (.length (first grid)))]
              (average-colors (neighbors-colors grid x y radius)))))))


(defn main []
  (let [size 600
        max-points 40
        set-points (random-points max-points size size)
        easy-draw (fn [f-dist & name]
                    (draw (calculate-colors
                            size
                            (get-closest-point-fn f-dist set-points))
                          size))
        draw-grid (fn [grid] (draw grid size))

        grid (calculate-colors size (get-closest-point-fn gcp2 set-points))
        blurred-grid (pp-blur grid :radius 4)
        edged-blurred-grid (pp-edges blurred-grid :radius 2)
        ]

    (easy-draw d-euclidean "d-euclidean")
    (easy-draw d-min "d-min")

    (easy-draw (fn [x1 y1 x2 y2]
                  (if (even? (int (d-euclidean x1 y1 x2 y2)))
                    Double/MAX_VALUE
                    (d-euclidean x1 y1 x2 y2))) "my-little-scanline")

    (def scanline-min-max-distance (compose d-euclidean (scanline d-min) (scanline d-max)))
    (easy-draw scanline-min-max-distance "scanline-min-max-distance")
    (easy-draw (compose d-euclidean noise-blur))
    ))


(main)
