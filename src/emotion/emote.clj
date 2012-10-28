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

;;(defrecord node
;;    [label, position, velocity])

(defrecord link
    [strength, begin, end])

(def node-list (ref [{:label "Hope" :position {:x 500 :y 50} :velocity {:x 0 :y 0}}
                {:label "Fear" :position {:x 55 :y 400} :velocity {:x 3 :y 0}}
                {:label "Vegetables" :position {:x 345 :y 234} :velocity {:x 0 :y 0}}]))

(def link-list (ref [(link. 10.0 (first @node-list) (second @node-list))]))

(defn make-node [label pos vel]
  {:label label :position pos :velocity vel})

(defn add-node [n-list n]
  (conj n-list n))

(defn make-and-add-node [n-list]
  (add-node n-list (make-node "Love" {:x 55 :y 55} {:x 0 :y 0})))

(defn op-map
  "Takes a set of {:x :y} spatial vectors and returns the result of applying the function op, where op expects a map of {:x :y} arguments. Useful for adding, subtracting, etc."
  ([op & vecs]
     (apply merge-with op vecs)))

(defn de-vec ""
 ([vec]
    (list (:x vec) (:y vec))
    ))

(defn vec-dist "Returns the distance between the two vectors."
  ([v1 v2]
     (apply quil.core/dist (flatten (list (de-vec v1) (de-vec v2))))))

(defn op-vec "Perform op on vectors, where op expects an unrolled seq of x y x y arguments."
  [op & vecs]
  (apply op (flatten (list (map de-vec vecs)))))

;; update node physics:
;; if pinned, velocity = 0
;; velocity = (* (+ velocity gravity) damping)
;; position = (+ velocity position)

;;update link physics:
;;get distance, diastance vector
;;normalize distance vector
;;dist = (* (- dist spring-length) spring-constant)
;;end-node.velocity.x = (+ end-node.velocity.x (* dist dist.x)) ;apply force

(defn update-node [the-node]
  (let [gravity {:x 0 :y 0}
        dampening {:x 1 :y 1}
        v (op-map * (op-map + (:velocity the-node) gravity) dampening)
        p (op-map + v (:position the-node))
        ]
    (println p)
    (assoc the-node :position p :velocity v)
    ))

(defn update-link [lnk list-of-nodes]
  (let [dist-vec (op-map - (:pos (:begin lnk)) (:pos (:end lnk)))
        distance (op-vec dist (:pos (:begin lnk)) (:pos (:end lnk)))
        spring-constant 0.02
        pull     (* (- distance (:strength lnk)) spring-constant)]
    ;; (find the node in the list of nodes)
    (vec (map #(if (= (:label %) (:label (:begin lnk)))
                 (assoc % :position (op-map + {:x 1 :y 0} (:position %)))
                 %)))
    ))

;;(defn node-apply-force [the-node force-vec]
;;  )

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
        x (+ (:x (:position (first @node-list))) (random 500))
        y (+ (:y (:position (first @node-list))) (* tim 135))
        size 15
        atext "Node Name"
        strg tim
        per-color (color 213 62 79)]

    (dosync (alter node-list (fn[x] (map #(update-node %) x))))
;;    (dosync (alter link-list...
  
    (draw-node-link x y 500 500 (abs tim) (lerp-color (color 200) per-color (abs tim)))
    (draw-inter-link x y 0 0 (abs tim))

   (let [closest (find-closest-node (mouse-x) (mouse-y) @node-list)]
     (draw-node-dot (:x (:position closest))
                    (:y (:position closest))
                    (* size 1.4)
                    (color 40 40 20)));highlighting
   (dorun (map #(draw-node-link
                  (:x (:position (:begin %))) (:y (:position (:begin %)))
                  (:x (:position (:end %)))   (:y (:position (:end %)))
                  (* (:strength %) 0.01)
                  per-color)
                @link-list))
   (dorun (map #(draw-node-dot (:x (:position %)) (:y (:position %)) size per-color) @node-list))
   (dorun (map #(draw-node-text (:x (:position %)) (:y (:position %)) size (:label %)) @node-list))
    
   (draw-node-dot x y size (lerp-color per-color (color 255) 0.0))
   (draw-node-text x y size (str (abs tim)))
   (draw-person-circle x y 200 per-color)))