(ns ngrad.core
  (:require [lanterna.screen :as s]
            [clojure.string :as str]))

; World/screen state
; map instead of vector seems excessive but probably will be useful in the
; future
(def world (ref {}))
(def world-row-widths (ref []))
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

(def walkable-object? #{"\\" "_" "|" "/"})

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

(defn wraparound
  "Very simplistic, physically incorrect wraparound algorithm for mountain."
  [x y]
  ; don't correct anything if there's no such row in world-row-widths
  (if (or (not (>= y 0)) (not (< y (count @world-row-widths))))
    [x y]
    ; decrement because we don't want to treat right edge as an ordinary square
    [(mod x (dec (get @world-row-widths y))) y]))

(defn mirror-map-edge
  [square]
  (let [ch (:ch square)]
    (case ch
      "\\" "/"
      "/" "\\"
      ch)))

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
(defn screen-to-world
  [x y]
  (let [center-x (quot @canvas-cols 2)
        center-y (quot @canvas-rows 2)
        delta-x (- center-x @player-x)
        delta-y (- center-y @player-y)]
    [(- x delta-x) (- y delta-y)]))

(defn get-rendered-square
  [screen-x screen-y]
  (let [[world-x world-y] (apply wraparound (screen-to-world screen-x screen-y))]
    (if (or (not (>= world-y 0)) (not (< world-y (count @world-row-widths))))
      " "
      (let [square (@world [world-x world-y])
            center-x (quot @canvas-cols 2)
            width (get @world-row-widths world-y)
            screen-width (quot width 2)
            left-corner (- center-x (quot screen-width 2) (rem screen-width 2))
            right-corner (+ center-x (quot screen-width 2))]
        (if (= screen-x left-corner)
          (mirror-map-edge (@world [(dec width) world-y]))
          (if (= screen-x right-corner)
            (:ch (@world [(dec width) world-y]))
            (if (and (> screen-x left-corner)
                     (< screen-x right-corner)
                     (some? square))
              (:ch square)
              " ")))))))

(defn render
  "Draw the world and the player on the screen."
  []
  (dosync
   ; draw the world
   (doseq [x (range @canvas-cols)
           y (range @canvas-rows)]
     (s/put-string @screen x y (get-rendered-square x y)))
   ; draw the player in center of the canvas
   (let [center-x (quot @canvas-cols 2)
         center-y (quot @canvas-rows 2)]
     (s/put-string @screen center-x center-y "i")
     (s/move-cursor @screen center-x center-y)))
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
      \4 [:move :left]
      \2 [:move :down]
      \8 [:move :up]
      \6 [:move :right]
      \7 [:move :up-left]
      \9 [:move :up-right]
      \1 [:move :down-left]
      \3 [:move :down-right]
      \h [:move :left]
      \j [:move :down]
      \k [:move :up]
      \l [:move :right]
      \y [:move :up-left]
      \u [:move :up-right]
      \b [:move :down-left]
      \n [:move :down-right]
      [nil nil])))

(defn walkable?
  "Does bounds checking via map and ensures the player doesn't walk through
   solid objects, so a player might not actually end up moving."
  [x y]
  (let [dest (@world [x y])]
    (and (some? dest) (walkable-object? (:ch dest)))))

(defmulti handle-command
  (fn [command _] command))

(defmethod handle-command nil [_ _]
  nil)

(defmethod handle-command :move [_ dir]
  "Move the player in the given direction."
  (dosync
   (let [[x y] (apply wraparound (calc-coords @player-x @player-y dir))]
     (when (walkable? x y)
       (ref-set player-x x)
       (ref-set player-y y)))))

; World creation
(defn array-to-world [array]
  ((fn [world widths col row index]
     (if (= (count array) index)
       [world widths]
       (let [ch (get array index)
             next-index (inc index)]
         (cond
           ; ignore it
           (= ch \return) (recur world widths col row next-index)
           ; go to next row
           (= ch \newline) (recur world (conj widths col) 0 (inc row) next-index)
           ; add square
           :else (recur (-> world
                            (assoc [col row] (make-square (str ch))))
                        widths (inc col) row next-index)))))
   {} [] 0 0 0))

(defn create-world []
  (let [spawn-text (slurp "assets/spawn.txt")
        [x y] (int-array (map #(Integer/parseInt %) (str/split-lines spawn-text)))
        [local-world local-widths] (array-to-world (slurp "assets/map.txt"))]
    (dosync (ref-set world local-world)
            (ref-set world-row-widths local-widths)
            (ref-set player-x x)
            (ref-set player-y y))))

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
