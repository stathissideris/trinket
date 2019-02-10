(ns trinket.mouse
  (:import [java.awt.event MouseListener MouseMotionListener MouseWheelListener MouseEvent MouseWheelEvent]))


(defn listener [{:keys [clicked entered exited pressed released dragged moved wheel-moved]}]
  (proxy [MouseListener MouseMotionListener MouseWheelListener] []
    (mouseClicked [e]
      (when clicked (clicked e)))
    (mouseEntered [e]
      (when entered (entered e)))
    (mouseExited [e]
      (when exited (exited e)))
    (mousePressed [e]
      (when pressed (pressed e)))
    (mouseReleased [e]
      (when released (released e)))
    (mouseDragged [e]
      (when dragged (dragged e)))
    (mouseMoved [e]
      (when moved (moved e)))
    (mouseWheelMoved [^MouseWheelEvent e]
      (when wheel-moved (wheel-moved e)))))
