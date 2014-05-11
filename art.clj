(ns art)

(defn random-points [num-points x y]
  (for [_ (range num-points)]
    [(rand-int x) (rand-int y) (rand-int 255) (rand-int 255) (rand-int 255)]))

(def MAX-POINTS 10)
(def X 100)
(def Y 100)
(def set-points (random-points MAX-POINTS X Y))

(defn make-frame-graphics [x y]
  (let [frame (java.awt.Frame.)]
    (.setSize frame (java.awt.Dimension. x y))
    (.setVisible frame true)
    (.getGraphics frame)))

(defn abs [x]
  (if (>= x 0) x (- x)))

(defn distance-average-random-negative [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (zero? (rand-int 2)) (- average) average)))

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

(defn abstract-scanline [f-scanline-dist f-scanline-shape f-dist x1 y1 x2 y2]
  "Usage: (abstract-scanline distance-evener distance-average x1 y1 x2 y2)"
        (if (f-scanline-shape (f-scanline-dist x1 y1 x2 y2))
          (* 1000 (f-dist x1 y1 x2 y2))
          (f-dist x1 y1 x2 y2)))


;;;;;;;;;;;;;;;
;util functions
;;;;;;;;;;;;;;;

(defn divisible-by-fn [by-what]
  "Curry function that will test if a number is divisible by-something."
  (fn [number] (let [quotient (/ (int number) by-what)]
                 (= (int quotient) quotient))))

(defn square [x] (* x x))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this] (if (< (f min) (f this)) min this))
            coll)))

;;;;;;;;;;;;;;;;;;;
;distance functions
;;;;;;;;;;;;;;;;;;;

(defn distance-evener [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (even? (+ x1 y1 x2 y2)) (* 2 average) average)))

(defn distance-average-random-double [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (zero? (rand-int 2)) (* 2 average) average)))

(defn distance-average-random-triple [x1 y1 x2 y2]
  (let [average (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2)]
    (if (zero? (rand-int 2)) (* 3 average) average)))

(defn distance-average [x1 y1 x2 y2]
  (/ (+ (abs (- x2 x1)) (abs (- y2 y1))) 2))

(defn distance-average-mixmatch [x1 y1 x2 y2]
  (/ (+ (abs (- x2 y1)) (abs (- y2 x1))) 2))

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

(defn draw-point [gfx [x y] [r g b]]
  (do (.setColor gfx (java.awt.Color. r g b))
      (.fillRect gfx x y 1 1)))

(defn draw-circle [gfx [x y] [r g b] radius]
  (do (.setColor gfx (java.awt.Color. r g b))
      (.drawOval gfx (- x radius) (- y radius) (* 2 radius) (* 2 radius))))

(defn fill-frame [gfx closest-point-fn]
  (doseq [x (range X) y (range Y)]
    (let [[x2 y2 r g b] (closest-point-fn x y)]
      (if (= [x y] [x2 y2])
        (draw-point gfx [x y] [0 0 0])
        (draw-point gfx [x y] [r g b])))))

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
    (apply compose (cons (fn [x1 y1 x2 y2] ((first fs) f x1 y1 x2 y2)) (rest fs)))))

(def dd (compose distance-min-max blur))

(defn main []
  (fill-frame (make-frame-graphics X Y)
              (get-closest-point-fn distance4 set-points))

  (fill-frame (make-frame-graphics X Y)
              (get-closest-point-fn dd set-points))
)

(main)

;for each pair of points in set-points, draw a transparent(?) line, or
;curve
;
;Do this sort of personal-space simulation in real-time, with an led floor and
;either pressure pads in the floor or good computer vision / kinects

;visualize each distance function by plotting distances from a point, with a
;gradient, similar to topology maps
