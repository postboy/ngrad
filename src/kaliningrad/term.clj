(ns kaliningrad.term
  (:import java.nio.charset.Charset
           com.googlecode.lanterna.TerminalFacade
           com.googlecode.lanterna.screen.Screen
           com.googlecode.lanterna.terminal.Terminal
           com.googlecode.lanterna.input.Key))

(def key-kinds {com.googlecode.lanterna.input.Key$Kind/NormalKey :normal
                com.googlecode.lanterna.input.Key$Kind/Escape :esc
                com.googlecode.lanterna.input.Key$Kind/Backspace :bs
                com.googlecode.lanterna.input.Key$Kind/ArrowLeft :left
                com.googlecode.lanterna.input.Key$Kind/ArrowRight :right
                com.googlecode.lanterna.input.Key$Kind/ArrowUp :up
                com.googlecode.lanterna.input.Key$Kind/ArrowDown :down
                com.googlecode.lanterna.input.Key$Kind/Insert :ins
                com.googlecode.lanterna.input.Key$Kind/Delete :del
                com.googlecode.lanterna.input.Key$Kind/Home :home
                com.googlecode.lanterna.input.Key$Kind/End :end
                com.googlecode.lanterna.input.Key$Kind/PageUp :page-up
                com.googlecode.lanterna.input.Key$Kind/PageDown :page-down
                com.googlecode.lanterna.input.Key$Kind/Tab :tab
                com.googlecode.lanterna.input.Key$Kind/ReverseTab :reverse-tab
                com.googlecode.lanterna.input.Key$Kind/Enter :cr
                com.googlecode.lanterna.input.Key$Kind/Unknown :unknown
                com.googlecode.lanterna.input.Key$Kind/CursorLocation :cursor-location})


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
    (let [kind (key-kinds (.getKind k))]
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
