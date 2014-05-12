(ns art
  (:import (javax.swing JFrame JLabel)
           (java.awt Graphics Dimension Color)
           (java.awt.image BufferedImage)))

(def MAX-POINTS 40)
(def SIZE 600)

(defn draw-circle [gfx x y radius]
  (.drawOval gfx (- x radius) (- y radius) (* 2 radius) (* 2 radius)))

(defn paint-canvas [graphics size closest-point-fn]
  (doseq [y (range size)
          x (range size)]
    (let [[_ _ r g b] (closest-point-fn x y)]
      ; can maybe save some time if I store the color object instead of
      ; creating it for every point anew
      (.setColor graphics (Color. r g b))
      (.drawLine graphics x y x y))))

(defn draw [& {:keys [size closest-point-fn]
               :or {size 250 closest-point-fn (fn [x y] [3 14 0 255 128])}}]
  (let [image (BufferedImage. size size BufferedImage/TYPE_INT_RGB)
        canvas (proxy [JLabel] [] (paint [g] (.drawImage g image 0 0 this)))]
    (paint-canvas (.createGraphics image) size closest-point-fn)
    (doto (JFrame.)
      (.add canvas)
      (.setSize (Dimension. size size))
      (.show))))


;;;;;;;;;;;;;;;
;util functions
;;;;;;;;;;;;;;;

(defn random-points [num-points x y]
  (for [_ (range num-points)]
    [(rand-int x) (rand-int y) (rand-int 256) (rand-int 256) (rand-int 256)]))

(defn divisible-by-fn [by-what]
  "Curry function that will test if a number is divisible by-something."
  (fn [number] (let [quotient (/ (int (mod number 1000000)) by-what)]
                 (= (int quotient) quotient))))

(defn square [x] (* x x))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this] (if (< (f min) (f this)) min this))
            coll)))

(defn abs [x]
  (if (>= x 0) x (- x)))


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

(defn d-evener [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (even? (+ x1 y1 x2 y2)) (* 2 average) average)))

(defn d-avg [x1 y1 x2 y2]
  (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2))

(defn d-avg-mismatch [x1 y1 x2 y2]
  (/ (+ (abs (- x2 y1)) (abs (- y2 x1))) 2))

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

(defn d-avg-dimension [x1 y1 x2 y2]
  "Distance that averages the distance in the x direction and the y direction"
  (let [xdist (abs (- x2 x1)) ydist (abs (- y2 y1))]
    (/ (+ xdist ydist) 2)))

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


(def busy (compose d-min (scanline d-euclidean)))
(def scanny (compose d-max (scanline d-euclidean)))
(def noisy (compose d-min (scanline + :width 5) noise-blur))

(def gcp1 (compose d-min (scanline d-min :width 5)))
(def gcp2 (compose d-max (scanline d-linear-r :width 2)))
(def gcp3 (compose d-max (scanline d-euclidean-r :width 4)))
(def gcp4 (compose d-max (scanline d-euclidean-r)))
(def gcp5 (compose d-max (scanline d-min :width 3)))
(def gcp6 (compose d-max (scanline d-euclidean)))
(def gcp7 (compose d-max (scanline d-avg-rand-dbl)))
(def gcp8 (compose d-max (scanline d-min :width 3)))

(def d1 (compose d-min-max (scanline +)))
(def d2 (compose d-min-max (scanline *)))
(def d3 (compose d-min-max (scanline +) (scanline *)))
(def d4 (compose d-min-max (scanline + :width 3) (scanline * :width 3)))

(def t1 (compose d-min (scanline + :width 5) noise-blur))
(def t2 (compose d-min noise-blur (scanline + :width 5)))

(defn main []
  (let [set-points (random-points MAX-POINTS SIZE SIZE)
        easy-draw (fn [f-dist]
                    (draw :size SIZE
                          :closest-point-fn (get-closest-point-fn f-dist set-points)
                    ))]
    (easy-draw d1)
    (easy-draw d2)
    (easy-draw d3)
    (easy-draw d4)
))


(main)

;What I really want is a framework that makes it very simple to do things.
;for each point set color color-func
;for each pair of points, connect by a line of color color-func, opacity 3
;
;Do this sort of personal-space simulation in real-time, with an led floor and
;either pressure pads in the floor or good computer vision / kinects

;visualize each distance function by plotting distances from a point, with a
;gradient, similar to topology maps
