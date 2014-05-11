(ns art
  (:import (javax.swing JFrame JLabel)
           (java.awt Graphics Dimension Color)
           (java.awt.image BufferedImage)))

(def MAX-POINTS 10)
(def SIZE 400)

(defn paint-canvas [graphics size closest-point-fn]
  (doseq [y (range size)
          x (range size)]

    (let [[_ _ r g b] (closest-point-fn x y)]
      ; can maybe save some time if I store the color object instead of
      ; creating it for every point anew
      (.setColor graphics (Color. r g b))
      (.drawLine graphics x y x y))))

(defn draw [size closest-point-fn]
  (let [image (BufferedImage. size size BufferedImage/TYPE_INT_RGB)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g image 0 0 this)))]

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
  (fn [number] (let [quotient (/ (int number) by-what)]
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

; TODO: make transformers composable

(defn blur [f x1 y1 x2 y2]
  "Takes any distance function and essentially adds noise"
  (if (zero? (rand-int 2))
    (* 2 (f x1 y1 x2 y2))
    (f x1 y1 x2 y2)))

(defn pointy [f x1 y1 x2 y2]
  "Usage: (pointy distance-average x1 y1 x2 y2)"
  (if (even? (+ x2 y2))
    (* 2 (f x1 y1 x2 y2))
    (f x1 y1 x2 y2)))

(defn sum-scanline [f x1 y1 x2 y2]
  "Usage: (sum-scanline distance-average x1 y1 x2 y2)"
  (if (even? (+ x1 y1 x2 y2))
    (* 2 (f x1 y1 x2 y2))
    (f x1 y1 x2 y2)))

(defn overlap [f x1 y1 x2 y2]
  "Usage: (overlap distance-average x1 y1 x2 y2)"
  (if (even? (bit-xor x1 y1 x2 y2))
    (* 100 (f x1 y1 x2 y2))
    (f x1 y1 x2 y2)))

(defn xor-scanline [f x1 y1 x2 y2]
  "Usage: (xor-scanline distance-average x1 y1 x2 y2)"
  (if (even? (bit-xor x1 y1 x2 y2))
    (* 2 (f x1 y1 x2 y2))
    (f x1 y1 x2 y2)))

(defn mult-scanline [f x1 y1 x2 y2]
  "Usage: (mult-scanline distance-average x1 y1 x2 y2)"
  (if (even? (* x1 y1 x2 y2))
    (* 2 (f x1 y1 x2 y2))
    (f x1 y1 x2 y2)))

(defn scanline [f-shape scanline-width]
  (fn [f-dist x1 y1 x2 y2]
    (if ((divisible-by-fn scanline-width) (f-shape x1 y1 x2 y2))
      (* 1000 (f-dist x1 y1 x2 y2))
      (f-dist x1 y1 x2 y2))))

(defn abstract-scanline [f-scanline-dist f-scanline-shape f-dist x1 y1 x2 y2]
  "Usage: (abstract-scanline distance-evener distance-average x1 y1 x2 y2)"
        (if (f-scanline-shape (f-scanline-dist x1 y1 x2 y2))
          (* 1000 (f-dist x1 y1 x2 y2))
          (f-dist x1 y1 x2 y2)))


;;;;;;;;;;;;;;;;;;;
;distance functions
;;;;;;;;;;;;;;;;;;;

(defn distance-evener [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (even? (+ x1 y1 x2 y2)) (* 2 average) average)))

(defn distance-average [x1 y1 x2 y2]
  (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2))

(defn distance-average-mixmatch [x1 y1 x2 y2]
  (/ (+ (abs (- x2 y1)) (abs (- y2 x1))) 2))

(defn distance-average-random-negative [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (zero? (rand-int 2)) (- average) average)))

(defn distance-average-random-double [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (zero? (rand-int 2)) (* 2 average) average)))

(defn distance-average-random-triple [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (zero? (rand-int 2)) (* 3 average) average)))

(defn distance-linear-reversed [x1 y1 x2 y2]
  (- (+ (abs (- x2 x1)) (abs (- y2 y1)))))

(defn distance-linear [x1 y1 x2 y2]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn distance-euclidean [x1 y1 x2 y2]
  (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))

(defn distance-euclidean-reversed [x1 y1 x2 y2]
  (- (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

(defn distance-max-dimension [x1 y1 x2 y2]
  (let [xdist (abs (- x2 x1)) ydist (abs (- y2 y1))]
    (max xdist ydist)))

(defn distance-min-dimension [x1 y1 x2 y2]
  (let [xdist (abs (- x2 x1)) ydist (abs (- y2 y1))]
    (min xdist ydist)))

(defn distance-average-dimension [x1 y1 x2 y2]
  (let [xdist (abs (- x2 x1)) ydist (abs (- y2 y1))]
    (/ (+ xdist ydist) 2)))

(defn distance-min-max [x1 y1 x2 y2]
  (let [min-dist (distance-min-dimension x1 y1 x2 y2)
        max-dist (distance-max-dimension x1 y1 x2 y2)]
    (/ (+ min-dist max-dist) 2)))

(defn get-closest-point4 [x1 y1 set-points]
  (let [distance-curry (fn [[x2 y2 _ _ _]] (abstract-scanline distance-average-random-double (fn [x] (even? (int x))) distance-max-dimension x1 y1 x2 y2))]
    (min-by distance-curry set-points)))

; (defn get-closest-point [x1 y1 set-points]
;   (let [distance-curry (fn [[x2 y2 _ _ _]] (abstract-scanline distance-euclidean-reversed even? distance-max-dimension x1 y1 x2 y2))]
;     (min-by distance-curry set-points)))

; (defn get-closest-point [x1 y1 set-points]
;   (let [distance-curry (fn [[x2 y2 _ _ _]] (abstract-scanline distance-linear-reversed even? distance-max-dimension x1 y1 x2 y2))]
;     (min-by distance-curry set-points)))


(defn get-closest-point3 [x1 y1 set-points]
  (let [distance-curry (fn [[x2 y2 _ _ _]] (abstract-scanline distance-euclidean (fn [x] (even? (int x))) distance-max-dimension x1 y1 x2 y2))]
    (min-by distance-curry set-points)))

(defn get-closest-point2 [x1 y1 set-points]
  (let [distance-curry (fn [[x2 y2 _ _ _]] (abstract-scanline
                                             distance-min-dimension
                                             (divisible-by-fn 3)
                                             distance-max-dimension
                                             x1 y1 x2 y2))]
    (min-by distance-curry set-points)))

; (defn get-closest-point2 [x1 y1 set-points]
;   (let [distance-curry (fn [[x2 y2 _ _ _]] (abstract-scanline
;                                              distance-euclidean-reversed
;                                              (divisible-by-fn 4)
;                                              distance-max-dimension
;                                              x1 y1 x2 y2))]
;     (min-by distance-curry set-points)))

; (defn get-closest-point [x1 y1 set-points]
;   (let [distance-curry (fn [[x2 y2 _ _ _]] (abstract-scanline
;                                              distance-euclidean
;                                              (divisible-by-fn 2)
;                                              distance-max-dimension
;                                              x1 y1 x2 y2))]
;     (min-by distance-curry set-points)))

(defn get-closest-point-original [x1 y1 set-points]
  (let [distance-curry (fn [[x2 y2 _ _ _]] (abstract-scanline
                                             distance-min-dimension
                                             (divisible-by-fn 5)
                                             distance-min-dimension
                                             x1 y1 x2 y2))]
    (min-by distance-curry set-points)))

(defn get-closest-point-fn [f-distance set-points]
  (fn [x1 y1]
    (let [distance-to-set-point (fn [[x2 y2 _ _ _]] (f-distance x1 y1 x2 y2))]
      (min-by distance-to-set-point set-points))))

(defn distance4 [x1 y1 x2 y2]
  (abstract-scanline distance-euclidean (divisible-by-fn 2)
                     distance-max-dimension x1 y1 x2 y2))

;(compose-distance eucliden (scanlines :width 4 :shape distance-min) blur)

(defn compose [f & fs]
  (if (empty? fs) f
    (let [second-func-curried (fn [x1 y1 x2 y2] ((first fs) f x1 y1 x2 y2))]
      (apply compose (cons second-func-curried (rest fs))))))

(def d1 (compose distance-min-max sum-scanline))
(def d2 (compose distance-min-max mult-scanline))
(def d3 (compose distance-min-max sum-scanline mult-scanline))
(def d4 (compose distance-min-max sum-scanline mult-scanline))

(def gcp2 (compose distance-max-dimension (scanline distance-min-dimension 3)))

(defn main []
  (let [set-points (random-points MAX-POINTS SIZE SIZE)]
    ; (draw SIZE (fn [x1 x2] (get-closest-point2 x1 x2 set-points)))
    (draw SIZE (get-closest-point-fn gcp2 set-points))
    ; (draw SIZE (get-closest-point-fn d1 set-points))
    ; (draw SIZE (get-closest-point-fn d2 set-points))
    ; (draw SIZE (get-closest-point-fn d3 set-points))
))

(main)

;for each pair of points in set-points, draw a transparent(?) line, or
;curve
;
;Do this sort of personal-space simulation in real-time, with an led floor and
;either pressure pads in the floor or good computer vision / kinects

;visualize each distance function by plotting distances from a point, with a
;gradient, similar to topology maps
