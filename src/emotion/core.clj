(ns emotion.core
  (:use quil.core)
  (:require ;[emotion.dynamic :as dynamic]
   [emotion.emote :as emote]
   [emotion.vec :as vec]
   ))

;;(defsketch example
;;  :title "Oh so many grey circles"
;;  :setup dynamic/setup
;;  :draw dynamic/draw
;;  :size [323 200])

(defsketch emotion
  :title "What do you want?"
  :setup emote/setup
  :draw emote/draw
  :size [800 600]
  :renderer :java2d)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
