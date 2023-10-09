(in-package :xclhb-game-wireframe-3d-dungeon)

(defun init ()
  (init-cell-points-array *visible-length* *visual-angle* *window-width* *window-height*)
  (init-dungeon)
  (init-player-master)
  (init-enemy-master)
  (init-item-master))

(defun start ()
  (init)
  (open-window)
  (unwind-protect
       (start-game)
    (close-window))
  (values))
