(ns kaliningrad.term
  (:import java.nio.charset.Charset
           com.googlecode.lanterna.TerminalFacade
           com.googlecode.lanterna.screen.Screen
           com.googlecode.lanterna.terminal.Terminal
           com.googlecode.lanterna.input.Key)
    (:require [lanterna.constants :as c]))

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


(defn kill-screen [screen]
  (.stopScreen screen))

(defn refresh [screen]
  (.refresh screen))

(defn set-cursor [screen x y]
  (.setCursorPosition screen x y))

(defn get-key [screen]
  (when-let [k (.readInput screen)]
    (let [kind (c/key-codes (.getKind k))]
      (if (= kind :normal)
        (.getCharacter k)
        kind))))

(defn get-key-blocking [screen]
  (let [k (get-key screen)]
    (if (nil? k)
      (do
        (Thread/sleep 100)
        (recur screen))
      k)))
