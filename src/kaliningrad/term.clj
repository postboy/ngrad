(ns kaliningrad.term
  (:import java.nio.charset.Charset
           com.googlecode.lanterna.TerminalFacade
           com.googlecode.lanterna.screen.Screen
           com.googlecode.lanterna.terminal.Terminal
           com.googlecode.lanterna.input.Key)
  (:require [lanterna.screen :as s]
            [lanterna.constants :as c]))

(defn add-resize-listener [terminal f]
  (.addResizeListener terminal
                      (reify
                        com.googlecode.lanterna.terminal.Terminal$ResizeListener
                        (onResized [this newSize]
                          (f (.getRows newSize)
                             (.getColumns newSize))))))

(defn get-screen
  ([] (get-screen 20 20 identity))
  ([cols rows resized-fn]
   (let [terminal
         (TerminalFacade/createTerminal (Charset/forName "UTF-8"))
         #_(TerminalFacade/createSwingTerminal cols rows)
         screen (new Screen terminal)]
     (.startScreen screen)
     (add-resize-listener terminal resized-fn)
     screen)))
