(in-package :xclhb-game-wireframe-3d-dungeon)

(def *visual-angle* (* pi 1/3))

(def *visible-length* 5)

(def *cell-points-array*)

(def *direction* 0)

(def *floor*)



(defun screen-size-coefficient (visual-angle w)
  (/ w (* 2 (tan visual-angle))))

(defstruct cell
  (sides (vector nil nil nil nil)) ; nil :wall :door :locked-door
  ceiling ; nil :stairs
  floor
  (visible-p t)
  on-moved)

(defstruct (cell-points (:type vector))
  left-wall
  left-door
  right-wall
  right-door
  front-wall
  front-door
  ceiling-stairs
  floor-stairs)

(defun %make-cell-points (x z)
  (let ((left-x (- x 0.5))
        (right-x (+ x 0.5))
        (left-door-x (- x 0.3))
        (right-door-x (+ x 0.3))
        (near-z (- z 0.5))
        (far-z (+ z 0.5))
        (near-door-z (- z 0.3))
        (far-door-z (+ z 0.3)))
    (map 'vector
         (lambda (points)
           (concatenate 'vector points (vector (copy-seq (elt points 0)))))
         (make-cell-points :left-wall (vector (vector left-x -0.5 near-z)
                                              (vector left-x -0.5 far-z)
                                              (vector left-x 0.5 far-z)
                                              (vector left-x 0.5 near-z))
                           :left-door (vector (vector left-x -0.2 near-door-z)
                                              (vector left-x -0.2 far-door-z)
                                              (vector left-x 0.5 far-door-z)
                                              (vector left-x 0.5 near-door-z))
                           :right-wall (vector (vector right-x -0.5 near-z)
                                               (vector right-x -0.5 far-z)
                                               (vector right-x 0.5 far-z)
                                               (vector right-x 0.5 near-z))
                           :right-door (vector (vector right-x -0.2 near-door-z)
                                               (vector right-x -0.2 far-door-z)
                                               (vector right-x 0.5 far-door-z)
                                               (vector right-x 0.5 near-door-z))
                           :front-wall (vector (vector left-x -0.5 far-z)
                                               (vector right-x -0.5 far-z)
                                               (vector right-x 0.5 far-z)
                                               (vector left-x 0.5 far-z))
                           :front-door (vector (vector left-door-x -0.2 far-z)
                                               (vector right-door-x -0.2 far-z)
                                               (vector right-door-x 0.5 far-z)
                                               (vector left-door-x 0.5 far-z))
                           :ceiling-stairs (vector (vector left-door-x -0.5 near-door-z)
                                                   (vector left-door-x -0.5 far-door-z)
                                                   (vector right-door-x -0.5 far-door-z)
                                                   (vector right-door-x -0.5 near-door-z))
                           :floor-stairs (vector (vector left-door-x 0.5 near-door-z)
                                                 (vector left-door-x 0.5 far-door-z)
                                                 (vector right-door-x 0.5 far-door-z)
                                                 (vector right-door-x 0.5 near-door-z))))))

(defun %make-cell-points-0 (x near-z)
  (map 'vector
       (lambda (points)
         (map 'vector
              (lambda (p)
                (if (minusp (elt p 2))
                    (vector (elt p 0) (elt p 1) near-z)
                    p))
              points))
       (%make-cell-points x 0)))

(defun transform-point (point screen-size-coefficient w/2 h/2)
  (let ((x (elt point 0))
        (y (elt point 1))
        (z (elt point 2)))
    (x:make-point :x (+ (floor (* screen-size-coefficient (/ x z))) w/2)
                  :y (+ (floor (* screen-size-coefficient (/ y z))) h/2))))

(defun transform-cell-points (cell-points screen-size-coefficient w/2 h/2)
  (flet ((transform (p)
           (transform-point p screen-size-coefficient w/2 h/2)))
    (map 'vector (lambda (cp) (map 'vector #'transform cp)) cell-points)))

(defun make-cell-points-array (visible-length visual-angle screen-size-coefficient w h)
  (let* ((max-n (ceiling (* visible-length (tan visual-angle))))
         (w/2 (floor w 2))
         (h/2 (floor h 2))
         (cell-points-array (make-array (list (1+ visible-length) (1+ (* max-n 2))) :initial-element nil)))
    (loop for z from 1 to visible-length
          do (loop with n = (ceiling (* z (tan visual-angle)))
                   for x from (- n) to n
                   do (setf (aref cell-points-array z (+ x max-n))
                            (transform-cell-points (%make-cell-points x z) screen-size-coefficient w/2 h/2))))
    (let ((near-z (* 0.5 (tan (- (/ pi 2) visual-angle)))))
      (loop for x from -1 to 1
            do (setf (aref cell-points-array 0 (+ x max-n)) (transform-cell-points (%make-cell-points-0 x near-z) screen-size-coefficient w/2 h/2))))
    cell-points-array))

(defun init-cell-points-array (visible-length visual-angle window-width window-height)
  (setf *cell-points-array*
        (make-cell-points-array visible-length visual-angle
                                (screen-size-coefficient visual-angle window-width)
                                window-width window-height))
  (values))

(defun draw-floor ()
  (clear)
  (destructuring-bind (depth width) (array-dimensions *cell-points-array*)
    (labels ((draw-door-p (side)
               (member side '(:door :locked-door)))
             (draw-front (front cp)
               (when front
                 (fill-polygon (cell-points-front-wall cp))
                 (draw-lines (cell-points-front-wall cp))
                 (when (draw-door-p front)
                   (draw-lines (cell-points-front-door cp)))))
             (draw-left (left cp)
               (when left
                 (fill-polygon (cell-points-left-wall cp))
                 (draw-lines (cell-points-left-wall cp))
                 (when (draw-door-p left)
                   (draw-lines (cell-points-left-door cp)))))
             (draw-right (right cp)
               (when right
                 (fill-polygon (cell-points-right-wall cp))
                 (draw-lines (cell-points-right-wall cp))
                 (when (draw-door-p right)
                   (draw-lines (cell-points-right-door cp)))))
             (draw-stairs (cell cp)
               (when (eql (cell-ceiling cell) :stairs)
                 (draw-lines (cell-points-ceiling-stairs cp)))
               (when (eql (cell-floor cell) :stairs)
                 (draw-lines (cell-points-floor-stairs cp)))))
      (loop for z from (1- depth) downto 0
            with center = (floor width 2)
            do (loop for x from 0 below center
                     do (let ((cp (aref *cell-points-array* z x)))
                          (when cp
                            (let* ((cell (multiple-value-call #'aref *floor* (relative-point (- x center) z)))
                                   (sides (cell-sides cell)))
                              (draw-front (elt sides *direction*) cp)
                              (draw-left (elt sides (mod (1- *direction*) 4)) cp)
                              (draw-stairs cell cp)))))
               (loop for x from (1- width) above center
                     do (let ((cp (aref *cell-points-array* z x)))
                          (when cp
                            (let* ((cell (multiple-value-call #'aref *floor* (relative-point (- x center) z)))
                                   (sides (cell-sides cell)))
                              (draw-front (elt sides *direction*) cp)
                              (draw-right (elt sides (mod (1+ *direction*) 4)) cp)
                              (draw-stairs cell cp)))))
               (let* ((cell (multiple-value-call #'aref *floor* (relative-point 0 z)))
                      (cp (aref *cell-points-array* z center))
                      (sides (cell-sides cell)))
                 (draw-front (elt sides *direction*) cp)
                 (draw-left (elt sides (mod (1- *direction*) 4)) cp)
                 (draw-right (elt sides (mod (1+ *direction*) 4)) cp)
                 (draw-stairs cell cp)))
      (update-window))))

