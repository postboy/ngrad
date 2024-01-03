(ns kaliningrad.term
  (:import java.nio.charset.Charset
           com.googlecode.lanterna.TerminalFacade
           com.googlecode.lanterna.screen.Screen
           com.googlecode.lanterna.terminal.Terminal
           com.googlecode.lanterna.input.Key)
  (:require [lanterna.terminal :as t]))

(defn get-screen
  ([] (get-screen 20 20 identity))
  ([cols rows resized-fn]
   (let [terminal
         (TerminalFacade/createTerminal (Charset/forName "UTF-8"))
         #_(TerminalFacade/createSwingTerminal cols rows)
         screen (new Screen terminal)]
     (.startScreen screen)
     (t/add-resize-listener terminal resized-fn)
     screen)))
