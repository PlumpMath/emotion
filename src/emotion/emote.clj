(ns emotion.emote
  (:use quil.core
        [quil.helpers.drawing :only [line-join-points]]
        [quil.helpers.seqs :only [range-incl]]
        ))

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

(def node-list (ref [{:label "Alice"      :position {:x 200 :y 200} :velocity {:x 0 :y 0}}
                     {:label "Bob"        :position {:x 200 :y 201} :velocity {:x 0 :y 0}}
                     {:label "Carol"      :position {:x 200 :y 202} :velocity {:x 0 :y 0}}
                     {:label "A: Bob"     :position {:x 200 :y 203} :velocity {:x 0 :y 0}}
                     {:label "B: Alice"   :position {:x 200 :y 21} :velocity {:x 0 :y 0}}
                     {:label "Dreams"     :position {:x 200 :y 22} :velocity {:x 0 :y 0}}
                     {:label "Weather"    :position {:x 200 :y 23} :velocity {:x 0 :y 0}}
                     {:label "Greed"      :position {:x 200 :y 204} :velocity {:x 0 :y 0}}
                     {:label "Love"      :position {:x 20 :y 200} :velocity {:x 0 :y 0}}
                     {:label "Hope"       :position {:x 500 :y 50}  :velocity {:x 8 :y 8}}
                     {:label "Fear"       :position {:x 55 :y 400}  :velocity {:x 8 :y -9}}
                     {:label "Vegetables" :position {:x 345 :y 234} :velocity {:x -3 :y 3}}]))

(def link-list (ref [(link. 10.0 "Alice" "A: Bob")
                     (link. 10.0 "Bob" "B: Alice")
                     (link. 10.0 "Bob" "A: Bob")
                     (link. 10.0 "Alice" "B: Alice")
                     (link. 10.0 "Alice" "Fear")
                     (link. 10.0 "Alice" "Dreams")
                     (link. 10.0 "Alice" "Hope")
                     (link. 10.0 "Alice" "Greed")
                     (link. 10.0 "Alice" "Vegetables")
                     (link. 10.0 "Alice" "Weather")
                     (link. 10.0 "Bob" "Fear")
                     (link. 10.0 "Bob" "Dreams")
                     ]))

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

(defn de-vec "Convert {:x :y} map to flat list."
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
        dampening {:x 0.95 :y 0.95}
        v (op-map * (op-map + (:velocity the-node) gravity) dampening)
        p (op-map + v (:position the-node))
        ]
;;    (println p)
    (assoc the-node :position p :velocity v)
    ))

(defn find-node
  "Find node by name in list-of-nodes. If there are duplicate names, it returns the first one...I'd prefer to do it by pointer or id, but that comes later."
  ([name list-of-nodes]
      (first (filter #(= name (:label %)) @node-list))))

(defn node-apply-force [the-node force-vec]
  (let [nv (op-map + (:velocity the-node) force-vec)]
    (assoc the-node :velocity nv)))

(defn nl-apply-force [nlist the-node force]
    (map #(if (= (:label %) (:label the-node))
                 (node-apply-force % force)
                 %)
              nlist))

(defn normalize-vec [vv]
  (let [v (emotion.vec/normalise [(:x vv) (:y vv)])]
    {:x (first v) :y (second v)}))

(defn update-link [lnk list-of-nodes]
  (let [b (find-node (:begin lnk) list-of-nodes)
        e (find-node (:end lnk) list-of-nodes)
        dist-vec (op-map - (:position b) (:position e))
        distance (vec-dist (:position b) (:position e))
        spring-constant 0.02
        pull (* (- distance (:strength lnk)) spring-constant)
        dist-vec-norm (normalize-vec dist-vec)
        pull-vec (op-map * {:x pull :y pull} dist-vec-norm)
        pull-vec-inverse (op-map * {:x -1 :y -1} pull-vec)
        ]

;;    (dosync (alter list-of-nodes
;;                   (fn[x]
;;                     (conj
;;                      x
;;                      {:label "Test" :position {:x 50 :y 50} :velocity {:x 3 :y 1}}))))

    ;;(println dist-vec-norm)

    ;; This is terrible design. This is also a really quick prototype, so I don't care yet.
    (dosync (alter list-of-nodes
                   (fn[x]
                     (nl-apply-force x b pull-vec-inverse)))
            (alter list-of-nodes
                   (fn[x]
                     (nl-apply-force x e pull-vec))))
    list-of-nodes
    ))

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
    ;; (println x1 y1 x2 y2)
     (stroke-weight 3)
     (stroke 100)
     (line x1 y1 x2 y2))
  ([x1 y1 x2 y2 strength]
     ;;(println x1 y1 x2 y2 strength)
     (stroke-weight (+ 5 (* strength 2)))
     (stroke (- 180 (* strength 80)))
     (line x1 y1 x2 y2))
  ([x1 y1 x2 y2 strength per-color]
     ;;(println x1 y1 x2 y2 strength per-color)
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
    ;;(dosync (alter node-list (fn[x] (reduce #(update-link % node-list) @link-list))))
    ;;(update-link (first @link-list) @node-list)
    (dorun (map #(update-link % node-list) @link-list))
   ;; (dosync (alter node-list (fn[x] (map #(update-link % x) @link-list))))


    ;;    (dosync (alter node-list (fn[x] (map #(update-link link-list %) x))))
    ;;(dosync (alter link-list (fn[x2] (map #(update-link % node-list) x2))))
    ;(dosync (alter link-list (fn[x] (map #(update-link % node-list) x))))
  
    ;;(draw-node-link x y 500 500 (abs tim) (lerp-color (color 200) per-color (abs tim)))
   ;; (draw-inter-link x y 0 0 (abs tim))

   (let [closest (find-closest-node (mouse-x) (mouse-y) @node-list)]
     (draw-node-dot (:x (:position closest))
                    (:y (:position closest))
                    (* size 1.4)
                    (color 40 40 20)));highlighting
   (dorun (map (fn[lnk] 
                 (let [node-1 (find-node (:begin lnk) @node-list);(first (filter #(= (:begin lnk) (:label %)) @node-list))
                       node-2 (find-node (:end lnk) @node-list)];(first (filter #(= (:end lnk) (:label %)) @node-list))]
                   ;;(println node-2)
                   ;;(println (:position node-2))
                   (draw-node-link
                   (:x (:position node-1)) (:y (:position node-1))
                   (:x (:position node-2)) (:y (:position node-2))
                   (* (:strength lnk) 0.01)
                   per-color
                   )))
                 @link-list))
   (dorun (map #(draw-node-dot (:x (:position %)) (:y (:position %)) size per-color) @node-list))
   (dorun (map #(draw-node-text (:x (:position %)) (:y (:position %)) size (:label %)) @node-list))
    
   ;;(draw-node-dot x y size (lerp-color per-color (color 255) 0.0))
   ;;(draw-node-text x y size (str (abs tim)))
   ;;(draw-person-circle x y 200 per-color)))
   ))