(in-package :xclhb-game-wireframe-3d-dungeon)

(defmacro def (name &optional value doc)
  `(progn
     (x:defglobal ,name nil ,doc)
     (setf ,name ,value)
     ',name))

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun random+1 (n)
  (1+ (random n)))

(defun copy-props (from to &rest keys)
  (dolist (key keys)
    (setf (getf to key) (getf from key))))

