Distance Art
============

Create random pretty pictures based on "distance" functions.
This is also called a [Voronoi Diagram](http://en.wikipedia.org/wiki/
Voronoi_diagram).

### So take me through it ###

Ok:

 1. We randomly generate a few points (x and y coordinates and an rgb
    value).
 2. Iterate through all possible points in our picture, and use a "distance"
    function to figure out which of the inital random points each point is
    closest to, and set the color of our current point to match it.
    - The reason I put "distance" in quotes is that a lot of the functions
      would make for terrible distance functions, but they make pretty
      pictures.
 3. Optionally add post-processing effects like blurring.
 4. Show a hopefully pretty picture.

### Examples ###

#### Euclidean ####

Let's start with a basic Euclidean distance function.  This is what you would
normally think of when trying to figure out the distance between two points,
based on the Pythagorean theorem.

```clojure
(defn d-euclidean [x1 y1 x2 y2]
  "Euclidean distance"
  (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))
```

```
............................
..............2.............
............................
...........1................
............................
............................
............................
............................
............................
............3...............
```

Using this metric, the closest point to point 1 is point 2.

```clojure
(easy-draw d-euclidean)
```

![pic](/screenshots/d-euclidean.png)

#### Min ####

To compare, we could define distance as the minimum of the distance in the
y direction, and the x direction,


```clojure
(defn d-min [x1 y1 x2 y2]
  "Distance that is the minimum of the distance in the x direction, and the y"
  (let [xdist (abs (- x2 x1)) ydist (abs (- y2 y1))]
    (min xdist ydist)))
```

```
............................
..............2.............
............................
...........1................
............................
............................
............................
............................
............................
............3...............
```

Using this metric, the closest point to point 1 is point 3, because the
distance in the x direction is 1. Point 2 is an 3 away in the x directoin and 2
in the y direction, giving it a distance of (min 2 3) == 2.

```clojure
(easy-draw d-min)
```

![pic](/screenshots/d-min.png)

#### Scanlines ####

We can get more creative and throw in an if statement into our distance
function:

```clojure
(defn my-little-scanline [x1 y1 x2 y2]
  "Scanlines!"
  (if (even? (int (d-euclidean x1 y1 x2 y2)))
    Double/MAX_VALUE
    (d-euclidean x1 y1 x2 y2))))
```

This creates a scanline effect:

```clojure
(easy-draw my-little-scanline)
```

![pic](/screenshots/my-little-scanline.png)

Neat!

But this scanline is based on euclidean distance.  In reality, you could use
any distance!

Meet `scanline`
```clojure
(defn scanline [f-shape & {:keys [width] :or {width 2}}]
  "Make scanline function using f-shape for shape."
  (fn [f-dist x1 y1 x2 y2]
    (if ((divisible-by-fn width) (f-shape x1 y1 x2 y2))
      Double/MAX_VALUE
      (f-dist x1 y1 x2 y2))))
```

So let's try it.

```clojure
; We use d-min for shape, and d-euclidean for distance.
(def scanline-min (scanline d-min))
(def scanline-distance (partial d-euclidean scanline-min))
; (easy-draw scanline-distance)
```

But let's not draw it just yet.

This might seem a little convoluded, but the nice thing about writing our
scanline functions like this is they can be passed to other scanline functions!

```clojure
; We use d-min for shape, and d-euclidean for distance.
(def scanline-min (scanline d-min))
(def scanline-max (scanline d-max))
(def scanline-min-max-distance (partial d-euclidean (partial scanline-min scanline-max)))
; (easy-draw scanline-min-max-distance)
```

Again, let's hold off on drawing it.

This is cool, but really verbose and hard to keep track of.  What we need is
a helper function that would let us easily compose all these distance functions
and scanlines together:

```clojure
(defn compose [f & fs]
  (if (empty? fs) f
    (let [second-func-curried (partial (first fs) f)]
      (apply compose (cons second-func-curried (rest fs))))))
```

Now we can write the same thing as above much more easily:

```clojure
(def scanline-min-max-distance (compose d-euclidean (scanline d-min) (scanline d-max)))
(easy-draw scanline-min-max-distance)
```

![pic](/screenshots/scanline-min-max-distance.png)


#### Noise ####

```clojure
(defn noise-blur [f x1 y1 x2 y2]
  "Takes any distance function and essentially adds noise"
  (if (zero? (rand-int 2))
    (* 2 (f x1 y1 x2 y2))
    (f x1 y1 x2 y2)))
```

```clojure
(easy-draw (compose d-euclidean noise-blur))
```

It's not always ugly, but when it is...

![pic](/screenshots/d-euclidean-noise.png)

### Post Processing ###

You can add blur (and others if you like) effects before drawing.

But I'm tired and sickish, so if you're interested, look through the source.
But beware: I don't actually know how to write clojure.
