(ns ngrad.core
  (:require [lanterna.screen :as s]
            [clojure.string :as str]))

; World/screen state
; map instead of vector seems excessive but probably will be useful in the
; future
(def world (ref {}))
(def player-x (ref 0))
(def player-y (ref 0))
(def canvas-cols (ref 0))
(def canvas-rows (ref 0))
(def screen (ref nil))

; Data structures
; record instead of char/string seems excessive but probably will be useful in
; the future
(defrecord Square [ch])

(defn make-square [ch] (new Square ch))

(def walkable-object? #{" " "."})

; Utility functions
(defn create-screen
  [resized-fn]
  (dosync (ref-set screen (s/get-screen :auto)))
  (s/start @screen)
  ; for some reason, this works better than setting :resize-listener argument
  ; to get-screen
  (s/add-resize-listener @screen resized-fn)
  (let [[cols rows] (s/get-size @screen)]
    (dosync (ref-set canvas-cols cols)
            (ref-set canvas-rows rows))))

(defn calc-coords
  "Calculate the new coordinates after moving dir from [x y].
   Does not do any bounds checking, so (calc-coords 0 0 :left) will
   return [-1 0] and let you deal with it."
  [x y dir]
  (case dir
    :left       [(dec x) y]
    :right      [(inc x) y]
    :up         [x (dec y)]
    :down       [x (inc y)]
    :up-left    [(dec x) (dec y)]
    :up-right   [(inc x) (dec y)]
    :down-left  [(dec x) (inc y)]
    :down-right [(inc x) (inc y)]))

; Rendering
; player will be in center of the canvas, so move everything accordingly
(defn translate-coordinates
  [x y]
  (let [center-x (quot @canvas-cols 2)
        center-y (quot @canvas-rows 2)
        delta-x (- center-x @player-x)
        delta-y (- center-y @player-y)]
    [(+ x delta-x) (+ y delta-y)]))

(defn inside-canvas?
  [x y]
  (and (>= x 0)
       (< x @canvas-cols)
       (>= y 0)
       (< y @canvas-rows)))

(defn render
  "Draw the world and the player on the screen."
  []
  (dosync
   ; clear screen
   (doseq [x (range @canvas-cols)
           y (range @canvas-rows)]
     (s/put-string @screen x y " "))
   ; draw the world
   (doseq [[[x y] square] @world]
     (let [[screen-x screen-y] (translate-coordinates x y)]
       (when (inside-canvas? screen-x screen-y)
         (s/put-string @screen screen-x screen-y (:ch square)))))
   ; draw the player in center of the canvas, no need to call inside-canvas?
   (let [[screen-x screen-y] (translate-coordinates @player-x @player-y)]
     (s/put-string @screen screen-x screen-y "i")
     (s/move-cursor @screen screen-x screen-y)))
  (s/redraw @screen))

; Input/command handling
(defn parse-input
  "Get a key from the user and return what command they want (if any).
   The returned value is a vector of [command-type data], where data is any
   extra metadata that might be needed (like the direction for a :move command)."
  []
  (let [k (s/get-key-blocking @screen)]
    (case k
      \q [:quit nil]
      :left [:move :left]
      :down [:move :down]
      :up [:move :up]
      :right [:move :right]
      \4 [:move :left]
      \2 [:move :down]
      \8 [:move :up]
      \6 [:move :right]
      \7 [:move :up-left]
      \9 [:move :up-right]
      \1 [:move :down-left]
      \3 [:move :down-right]
      [nil nil])))

(defn walkable?
  "Does bounds checking via map and ensures the player doesn't walk through
   solid objects, so a player might not actually end up moving."
  [x y]
  (let [dest (@world [x y])
        path-a (@world [@player-x y])
        path-b (@world [x @player-y])]
    ; destination and at least one of intermediate squares in case of diagonal
    ; moving should be walkable
    (and (some? dest) (walkable-object? (:ch dest))
         (or (and (some? path-a) (walkable-object? (:ch path-a)))
             (and (some? path-b) (walkable-object? (:ch path-b)))))))

(defmulti handle-command
  (fn [command _] command))

(defmethod handle-command nil [_ _]
  nil)

(defmethod handle-command :move [_ dir]
  "Move the player in the given direction."
  (dosync
   (let [[x y] (calc-coords @player-x @player-y dir)]
     (when (walkable? x y)
       (ref-set player-x x)
       (ref-set player-y y)))))

; World creation
(defn convert-array-to-world [array]
  ((fn [result col row index]
     (if (= (count array) index)
       result
       (let [ch (get array index)
             next-index (inc index)]
         (cond
           ; ignore it
           (= ch \return) (recur result col row next-index)
           ; go to next row
           (= ch \newline) (recur result 0 (inc row) next-index)
           ; add square
           :else (recur (-> result
                            (assoc [col row] (make-square (str ch))))
                        (inc col) row next-index)))))
   {} 0 0 0))

(defn create-world []
  (let [spawn-text (slurp "assets/town/spawn.txt")
        [x y] (int-array (map #(Integer/parseInt %) (str/split-lines spawn-text)))]
    (dosync (ref-set world (convert-array-to-world (slurp "assets/town/map.txt")))
            (ref-set player-x x)
            (ref-set player-y y))))

; Main
(defn game-loop []
  (render)
  (let [[command data] (parse-input)]
    (if (= command :quit)
      (s/stop @screen)
      (do
        (handle-command command data)
        (recur)))))

(defn handle-resize [cols rows]
  (dosync (ref-set canvas-cols cols)
          (ref-set canvas-rows rows))
  ; for some reason, (redraw) inside (render) is not enough
  (s/redraw @screen)
  ; we need to re-render the screen
  (render))

(defn -main [& _]
  (create-screen handle-resize)
  (create-world)
  (game-loop))
