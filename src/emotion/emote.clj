(ns emotion.emote
  (:use quil.core
        [quil.helpers.drawing :only [line-join-points]]
        [quil.helpers.seqs :only [range-incl]]))

;;; Nodes

;; node id
;; person it belongs to
;; current position
;; previous position
;; transition time
;; color
;; list of links

(defrecord node
    [label, position])

(defrecord link
    [strength, begin, end])

(def node-list [(node. "Hope" {:x 500 :y 50}) (node. "Fear" {:x 50 :y 400}) (node. "Vegetables" {:x 345 :y 234})])

(def link-list [(link. 0.1 (first node-list) (second node-list))])

(defn update-node [the-node]
  )

(defn node-apply-force [the-node force-vec]
  )

;;; interface
(defn find-closest-node
  "From the list of nodes, find the closest one to the given location."
  ([x-pos y-pos nodes]
;;     (sort-by :pos-x nodes))
     (first
      (sort-by #(dist x-pos y-pos (:x (:position %1)) (:y (:position %1)))
              ;;(dist x-pos y-pos (:pos-x %2) (:pos-y %2)))
       nodes))
     ))

(defn find-closest-link [x-pos y-pos links]
  )

;;; Drawing Commands

(defn mod-color
  "Modulates a color by a strength in range 0..1"
  ([col mod]
     (color col col col mod)))

;; draw node circle
(defn draw-node-dot
  ([x y size]
     (stroke 10 10 40)
     (stroke-weight 2)
     (fill 30 30 80)
     (ellipse x y size size))
  ([x y size per-color]
     (stroke per-color)
     (stroke-weight 2)
     (fill per-color)
     (ellipse x y size size))
  )

;; draw node text
(defn draw-node-text [x y size atext]
  "Draw text next to the specified node location"
                                        ;(text-mode :screen)
  (fill 0)
  (text-align :left :center)
  (text-mode :model)
  (text atext (+ x size) y)
  );(text "test"));; (+ x 40) y))
;; draw person cicle
(defn draw-person-circle
  "Draw the boundaries of the psyche"
  ([x y person-radius per-color]
     (stroke-weight 5)
     (let [radius    person-radius
           cent-x    x
           cent-y    y
           rads      (map radians (range-incl 0 360 5))
           xs        (map #(+ cent-x (* radius (cos %))) rads)
           ys        (map #(+ cent-y (* radius (sin %))) rads)]
       (stroke 0 30)
       (no-fill)
       (ellipse cent-x cent-y (* radius 2) (* radius 2))
       (stroke per-color)
       (dorun (map point xs ys)))))

;; draw node link
(defn draw-node-link
  "Draw link line between two nodes"
  ([x1 y1 x2 y2]
     (stroke-weight 3)
     (stroke 100)
     (line x1 y1 x2 y2))
  ([x1 y1 x2 y2 strength]
     (stroke-weight (+ 5 (* strength 2)))
     (stroke (- 180 (* strength 80)))
     (line x1 y1 x2 y2))
  ([x1 y1 x2 y2 strength per-color]
     (stroke-weight (+ 5 (* strength 2)))
     (stroke (lerp-color per-color (color 20 20 20) (abs strength)))
     (line x1 y1 x2 y2))
  )

;; draw person-link
(defn draw-inter-link
  "Draw a link between two nodes attached to separate people"
  ([x1 y1 x2 y2]
     (stroke-weight 10)
     (stroke 210)
     (line x1 y1 x2 y2))
  ([x1 y1 x2 y2 strength]
     (stroke-weight (+ 10 (* strength 6)))
     (stroke (- 200 (* strength 40)))
     (line x1 y1 x2 y2))
  )

(defn setup []
  (smooth)
  (frame-rate 30)
  (background 255)
  )

;; colors
;;213 62 79
;;252 141 89
;;254 224 139
;;255 255 191
;;230 245 152
;;153 213 148
;;50 136 189

(defn draw []
  (background 240)
  (let [tim (Math/sin (/ (millis) 1000))
        x (+ (:x (:position (first node-list))) (random 500))
        y (+ (:y (:position (first node-list))) (* tim 135))
        size 15
        atext "Node Name"
        strg tim
        per-color (color 213 62 79)]
    (draw-node-link x y 500 500 (abs tim) (lerp-color (color 200) per-color (abs tim)))
    (draw-inter-link x y 0 0 (abs tim))
    ;;(dorun (map (draw-node-dot 50 5 node-list))
    ;;(find-closest-node (mouse-x) (mouse-y) node-list)
   (let [closest (find-closest-node (mouse-x) (mouse-y) node-list)]
     (draw-node-dot (:x (:position closest)) (:y (:position closest)) (* size 1.4) (color 40 40 20));highlighting
     )
    (dorun (map #(draw-node-link
                  (:x (:position (:begin %))) (:y (:position (:begin %)))
                  (:x (:position (:end %))) (:y (:position (:end %)))
                  (:strength %)
                  per-color)
                link-list))
    (dorun (map #(draw-node-dot (:x (:position %)) (:y (:position %)) size per-color) node-list))
    (dorun (map #(draw-node-text (:x (:position %)) (:y (:position %)) size (:label %)) node-list))

    ;;(random 50)
    
    (draw-node-dot x y size (lerp-color per-color (color 255) 0.0))
    (draw-node-text x y size (str (abs tim)))
    (draw-person-circle x y 200 per-color)
    ))