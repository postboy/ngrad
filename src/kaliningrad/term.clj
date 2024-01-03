(ns kaliningrad.term
  (:require [lanterna.screen :as s]))

(defn get-screen
  [cols rows resized-fn]
  (let [screen (s/get-screen :auto)]
    (s/start screen)
    (s/add-resize-listener screen resized-fn)
    screen))
