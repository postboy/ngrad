(ns ngrad.core
  (:require [lanterna.screen :as s]
            [clojure.string :as string]
            [clojure.math :as math]))

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

(defn mirror-map-edge
  [square]
  (let [ch (:ch square)]
    (case ch
      "\\" "/"
      "/" "\\"
      ch)))

; fix coordinates so if you can go up then you can also go down by same squares
(defn recalculate-x
  [source-x source-y target-y]
  (let [source-line-width (dec (get @world-row-widths source-y))
        target-line-width (dec (get @world-row-widths target-y))]
    ; previous line is special: we ensure it doesn't move horizontally when you go up
    (if (not (= source-y (dec target-y)))
      (math/round (* (/ source-x source-line-width) target-line-width))
      (let [target-x-floor (int (math/floor (* (/ source-x source-line-width) target-line-width)))
            target-x-ceil (int (math/ceil (* (/ source-x source-line-width) target-line-width)))]
        (if (= source-x (math/round (* (/ target-x-floor target-line-width) source-line-width)))
          target-x-floor
          target-x-ceil)))))

; Rendering
; player will be in center of the canvas, so move everything accordingly
(defn screen-to-world
  [screen-x screen-y]
  (let [center-x (quot @canvas-cols 2)
        center-y (quot @canvas-rows 2)
        delta-x (- center-x @player-x)
        delta-y (- center-y @player-y)
        world-x (- screen-x delta-x)
        world-y (- screen-y delta-y)]
    ; don't correct anything if there's no such row in world-row-widths
    (if (or (not (>= world-y 0)) (not (< world-y (count @world-row-widths))))
      [world-x world-y]
      ; when user stands on the last column in the row there's last columns above and below him too
      ; despite the fact that all rows have different lengths
      (let [this-line-width (dec (get @world-row-widths world-y))
            this-line-center (recalculate-x @player-x @player-y world-y)
            ; modular arithmetics to wrap around the mountain map
            ; decrement because we don't want to treat right edge as an ordinary square
            corrected-world-x (mod (+ (- this-line-center center-x) screen-x) this-line-width)]
        [corrected-world-x world-y]))))

(defn calc-screen-coords
  "Calculate the new screen coordinates after moving dir from current position."
  [dir]
    (let [center-x (quot @canvas-cols 2)
          center-y (quot @canvas-rows 2)]
      (case dir
        :left       [(dec center-x) center-y]
        :right      [(inc center-x) center-y]
        :up         [center-x (dec center-y)]
        :down       [center-x (inc center-y)]
        :up-left    [(dec center-x) (dec center-y)]
        :up-right   [(inc center-x) (dec center-y)]
        :down-left  [(dec center-x) (inc center-y)]
        :down-right [(inc center-x) (inc center-y)])))

(defn get-rendered-square
  [screen-x screen-y]
  (let [[world-x world-y] (screen-to-world screen-x screen-y)]
    (if (or (not (>= world-y 0)) (not (< world-y (count @world-row-widths))))
      " "
      (let [square (@world [world-x world-y])
            center-x (quot @canvas-cols 2)
            width (get @world-row-widths world-y)
            screen-width (quot width 2)
            left-corner (- center-x (quot screen-width 2))
            right-corner (+ center-x (quot screen-width 2) (rem screen-width 2))]
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
  ;(println (inc @player-x) (inc @player-y))
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
   (let [[x y] (apply screen-to-world (calc-screen-coords dir))]
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
           (or (= ch \return) (= ch \`)) (recur world widths col row next-index)
           ; go to next row
           (= ch \newline) (recur world (conj widths col) 0 (inc row) next-index)
           ; add square
           :else (recur (-> world
                            (assoc [col row] (make-square (str ch))))
                        widths (inc col) row next-index)))))
   {} [] 0 0 0))

(defn create-world []
  (let [spawn-text (slurp "assets/spawn.txt")
        [x y] (int-array (map #(Integer/parseInt %) (string/split-lines spawn-text)))
        [local-world local-widths] (array-to-world (slurp "assets/map.txt"))]
    (dosync (ref-set world local-world)
            (ref-set world-row-widths local-widths)
            (ref-set player-x (dec x))
            (ref-set player-y (dec y)))))

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
