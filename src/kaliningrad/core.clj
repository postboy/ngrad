(ns kaliningrad.core
  (:require [lanterna.screen :as s]))

; Constants -------------------------------------------------------------------
(def rows 15)
(def cols 40)

(def welcome-message
  ["Welcome to Kaliningrad!"
   ""
   "Go for a walk in Russian city located in the heart of Europe!"
   ""
   "There is no winning, losing, or saving. Press ? to see help anytime."
   ""
   "Press any key to begin."])

(def help-message
  [" ------ HELP ------- "
   " arrow keys - move   "
   " q          - quit   "
   " ?          - help   "
   "                     "
   " -- press any key -- "])

(def walkable? #{" " "."})

; World/screen state ----------------------------------------------------------
; map instead of vector seems excessive but probably will be useful in the
; future
(def world (ref {}))
(def player-x (ref 0))
(def player-y (ref 0))
(def canvas-rows (ref 24))
(def canvas-cols (ref 80))

; Data structures -------------------------------------------------------------
; record instead of char/string seems excessive but probably will be useful in
; the future
(defrecord Square [ch])

(defn make-square [ch] (new Square ch))

; Utility functions -----------------------------------------------------------
(defn get-new-screen
  [cols rows resized-fn]
  (let [screen (s/get-screen :auto {:cols cols :rows rows})]
    (s/start screen)
    (s/add-resize-listener screen resized-fn)
    screen))

(defn draw-lines
  "Draw a sequence of lines down the left side of the screen."
  [screen lines]
  (loop [i 0
         [l & ls] lines]
    (when l
      (s/put-string screen 0 i l)
      (recur (inc i) ls)))
  (s/redraw screen))

(defn calc-coords
  "Calculate the new coordinates after moving dir from [x y].

  Does not do any bounds checking, so (calc-coords 0 0 :left) will
  return [-1 0] and let you deal with it."
  [x y dir]
  (case dir
    :left  [(dec x) y]
    :right [(inc x) y]
    :up    [x (dec y)]
    :down  [x (inc y)]))

; Rendering -------------------------------------------------------------------
(defn render
  "Draw the world and the player on the screen."
  [screen]
  (dosync
   (doseq [y (range @canvas-rows)
           x (range @canvas-cols)]
     (s/put-string screen x y " "))
   (doseq [y (range rows)
           x (range cols)
           :let [{:keys [ch]} (@world [x y])]]
     (s/put-string screen x y ch))
   (s/put-string screen @player-x @player-y "@")
   (s/move-cursor screen @player-x @player-y))
  (s/redraw screen))

; Input/command handling ------------------------------------------------------
(defn parse-input
  "Get a key from the user and return what command they want (if any).

  The returned value is a vector of [command-type data], where data is any
  extra metadata that might be needed (like the direction for a :move command)."
  [screen]
  (let [k (s/get-key-blocking screen)]
    (case k
      \q [:quit nil]
      \? [:help nil]
      :left [:move :left]
      :down [:move :down]
      :up [:move :up]
      :right [:move :right]
      [nil nil])))

(defmulti handle-command
  (fn [command _ _] command))

(defmethod handle-command nil [_ _ _]
  nil)

(defmethod handle-command :help [_ screen _]
  "Draw a help message on the screen and wait for the user to press a key."
  (s/move-cursor screen 0 0)
  (draw-lines screen help-message)
  (s/get-key-blocking screen))

(defmethod handle-command :move [_ _ dir]
  "Move the player in the given direction.

  Does bounds checking and ensures the player doesn't walk through solid
  objects, so a player might not actually end up moving."
  (dosync
   (let [[x y] (calc-coords @player-x @player-y dir)
         x (max 0 x)
         x (min x (dec cols))
         y (max 0 y)
         y (min y (dec rows))]
     (when (walkable? (:ch (@world [x y])))
       (ref-set player-x x)
       (ref-set player-y y)))))

; World generation ------------------------------------------------------------
(defn convert-array-to-world [result array col row next-index]
  (if (= (count array) next-index)
    result
    (let [ch (get array next-index)]
      (cond
        ; ignore it
        (= ch \return) (recur result
                              array
                              col
                              row
                              (inc next-index))
        ; go to next row
        (= ch \newline) (recur result
                               array
                               0
                               (inc row)
                               (inc next-index))
        ; add square
        :else (recur (-> result
                         (assoc [col row] (make-square (str ch))))
                     array
                     (inc col)
                     row
                     (inc next-index))))))

(defn generate-world []
  (dosync (ref-set world
                   (convert-array-to-world {}
                                           (slurp "data/map.txt")
                                           0 0 0))))

; Main ------------------------------------------------------------------------
(defn intro [screen]
  (draw-lines screen welcome-message)
  (s/get-key-blocking screen))

(defn game-loop [screen]
  (render screen)
  (let [[command data] (parse-input screen)]
    (if (= command :quit)
      (s/stop screen)
      (do
        (handle-command command screen data)
        (recur screen)))))

(defn handle-resize [rows cols]
  (dosync
   (ref-set canvas-rows rows)
   (ref-set canvas-cols cols)))

(defn -main [& _]
  (let [screen (get-new-screen (ref canvas-cols) (ref canvas-rows) handle-resize)]
    (generate-world)
    (intro screen)
    (game-loop screen)))
