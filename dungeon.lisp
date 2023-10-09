(in-package :xclhb-game-wireframe-3d-dungeon)

(defun draw-map ()
  (let* ((left 32)
         (top 32)
         (cell-size 30)
         (right (+ left (* (floor-cols) cell-size)))
         (bottom (+ top (* (floor-rows) cell-size)))
         (wall-thickness 3)
         (wall-width (- cell-size (* 2 wall-thickness))))
    (labels ((%draw-rect (x y w h c)
               (set-color c)
               (set-fill-style x:+fill-style--solid+)
               (fill-rect x y w h))
             (draw-wall (x y w h side)
               (let ((color (case side
                              (:wall #xaaaaaa)
                              (:door #xff2222)
                              (:locked-door #x6666ff)
                              (otherwise nil))))
                 (when color
                   (%draw-rect x y w h color))))
             (rect-params (i j d)
               (multiple-value-call (lambda (x y w h)
                                      (values (+ left x) (+ top y) w h))
                 (case d
                   (0 (values (+ wall-thickness (* j cell-size)) (* i cell-size) wall-width wall-thickness))
                   (1 (values (- (* (1+ j) cell-size) wall-thickness) (+ wall-thickness (* i cell-size)) wall-thickness wall-width))
                   (2 (values (+ wall-thickness (* j cell-size)) (- (* (1+ i) cell-size) wall-thickness) wall-width wall-thickness))
                   (3 (values (* j cell-size) (+ wall-thickness (* i cell-size)) wall-thickness wall-width)))))
             (extend-rect (x y w h)
               (values (+ x (- wall-thickness))
                       (+ y (- wall-thickness))
                       (+ w (* 2 wall-thickness))
                       (+ h (* 2 wall-thickness)))))
      (clear)
      (loop for i from 0 below (floor-rows)
            do (loop for j from 0 below (floor-cols)
                     for cell = (aref *floor* i j)
                     for sides = (cell-sides cell)
                     do (multiple-value-call #'draw-wall (rect-params i j 0) (elt sides 0))
                        (multiple-value-call #'draw-wall (rect-params i j 1) (elt sides 1))
                        (multiple-value-call #'draw-wall (rect-params i j 2) (elt sides 2))
                        (multiple-value-call #'draw-wall (rect-params i j 3) (elt sides 3))))
      (set-color #x555555)
      (set-line-style x:+line-style--on-off-dash+)
      
      (loop for i from 0 below (floor-rows)
            for y from top by cell-size
            do (draw-lines (vector (x:make-point :x left :y y) (x:make-point :x right :y y)))
               (setf *caret-x* 0)
               (setf *caret-y* y)
               (draw-string (format nil "~a" i)))
      (draw-lines (vector (x:make-point :x left :y (1- bottom)) (x:make-point :x right :y (1- bottom))))
      (setf *caret-y* 0)
      (loop for j from 0 below (floor-cols)
            for x from left by cell-size
            do (draw-lines (vector (x:make-point :x x :y top) (x:make-point :x x :y bottom)))
               (setf *caret-x* x)
               (draw-string (format nil "~a" j)))
      (draw-lines (vector (x:make-point :x (1- right) :y top) (x:make-point :x (1- right) :y bottom)))
      (set-color #xaaaa22)
      (set-line-width 3)
      (multiple-value-call #'draw-rect (extend-rect (+ left (* (px) cell-size)) (+ top (* (py) cell-size)) cell-size cell-size))
      (set-color #xff8800)
      ;;(multiple-value-call #'draw-rect (multiple-value-call #'extend-rect (rect-params (py) (px) *direction*)))
      (set-color #xffffff)
      (set-line-style x:+line-style--solid+)
      (set-line-width 1)))
  (update-window))

(defun %toggle-side (sides d)
  (setf (elt sides d)
        (getf '(nil :wall
                :wall :door
                :door :locked-door
                :locked-door nil)
              (elt sides d))))


(defun toggle-side (x y d both-side-p)
  (%toggle-side (cell-sides (aref *floor* y x)) d)
  (when both-side-p
    (setf (elt (cell-sides (multiple-value-call #'aref *floor* (%relative-point x y 0 1 d))) (mod (+ d 2) 4))
          (elt (cell-sides (aref *floor* y x)) d))))

(let ((both-side-p t))
  (defun map-maker-key-handler (e)
    (x:with-key-press-event (detail state) e
      (case (x:keycode->keysym *client* detail state)
        (:up (setf (py) (mod (1- (py)) (floor-rows))))
        (:down (setf (py) (mod (1+ (py)) (floor-rows))))
        (:left (setf (px) (mod (1- (px)) (floor-cols))))
        (:right (setf (px) (mod (1+ (px)) (floor-cols))))
        (#\w (toggle-side (px) (py) 0 both-side-p))
        (#\d (toggle-side (px) (py) 1 both-side-p))
        (#\s (toggle-side (px) (py) 2 both-side-p))
        (#\a (toggle-side (px) (py) 3 both-side-p))
        (#\space (setf both-side-p (not both-side-p)))))
    (draw-map)))

(defun floor-maker ()
  (open-window)
  (let ((pre-w *window-width*)
        (pre-h *window-height*))
    (set-window-size (+ 64 (* 30 (floor-cols))) (+ 64 (* 30 (floor-rows))))
    (x:set-event-handler *client* x:+key-press-event+ 'map-maker-key-handler)
    (draw-map)
    (start-game-loop)
    (setf *window-width* pre-w
          *window-height* pre-h)
    (setf *key-press-handler* nil)
    (close-window)))

(defun floor-to-form (floor)
  `(make-array ',(array-dimensions floor)
               :initial-contents
               (list ,@(loop for i from 0 below (array-dimension floor 0)
                             collect `(list ,@(loop for j from 0 below (array-dimension floor 1)
                                                    collect (let ((cell (aref floor i j)))
                                                              `(make-cell :sides (vector ,@(coerce (cell-sides cell) 'list))
                                                                          :ceiling ,(cell-ceiling cell)
                                                                          :floor ,(cell-ceiling cell)
                                                                          :visible-p ,(cell-visible-p cell)
                                                                          :on-moved ,(cell-on-moved cell)))))))))

(defun find-item (text item-id)
  (text `((,text))
        (lambda ()
          (select-yes-no '("ひろいますか？")
                         (lambda ()
                           (add-item item-id)
                           (move-mode))
                         'move-mode))))

(defun f1-find-trash-1 ()
  (when (= (random 5) 0)
    (find-item "ごみがおちている。" :trash-1)))

(defun f1-find-key-1 ()
  (unless (has-item-p :key-1)
    (find-item "かぎがおちている。" :key-1)))

(defun f3-find-key-2 ()
  (unless (has-item-p :key-2)
    (load-image (merge-pathnames "human-2.jpg" (merge-pathnames "resources/" (uiop:getcwd))))
    (draw-image-center)
    (select-simple '("きょうもはずしちまったのかい？")
                   (list (list :a1 "ファンタスティック"
                               (lambda ()
                                 (text `((,(format nil "~a さん、ファンタスティック!" (getf *player* :name))))
                                       'move-mode)))
                         (list :a2 "ジーニアス"
                               (lambda ()
                                 (text `((,(format nil "~a さん、ジーニアス!" (getf *player* :name))))
                                       'move-mode)))
                         (list :a3 "リンダリンダ"
                               (lambda () (text '(("AJCC!")) 'move-mode)))
                         (list :a4 "エエヤン"
                               (lambda ()
                                 (add-item :key-2)
                                 (text (list '("ええやん")
                                             (list (format nil "~a をてにいれた。" (item-name :key-2))))
                                       'move-mode))))
                   (lambda ()
                     (text '(("ほどほどにしとけよ。"))
                           'move-mode)))))

(defun f3-clear-item ()
  (unless (has-item-p :clear-item)
    (load-image (merge-pathnames "human-3.jpg" (merge-pathnames "resources/" (uiop:getcwd))))
    (draw-image-center)
    (add-item :clear-item)
    (text (list (list (format nil "げっ！！、~a" (getf *player* :name)))
                (list "なに？"
                      (format nil "~a をかえせだって？" (item-name :clear-item)))
                (list "かえさないと、かあちゃんにいいつける？")
                (list "まてっ！")
                (list "それだけはやめてくれ。")
                (list "わかった！わかったから！！")
                (list "ほら、かえすよ。")
                (list (format nil "~a は ~a をてにいれた。" (getf *player* :name) (item-name :clear-item)))
                (list "これでいいんだろ？")
                (list "ようがすんだらさっさとかえれ。ほら。"))
          (lambda ()
            (clear)
            (text '(("MISSION COMPLETE!"))
                   'move-mode)))))

(defun f1-message-1 ()
  (text '(("かべにスプレーでなにかかいてある。")
          ("ひ ろ し")
          ("ち か")
          ("3")
          ("か い")
          ("い る"))
        'move-mode))

(defun f1-battle ()
  (battle :human-4 'move-mode 'reset-game 'move-mode))

(defun f2-message-1 ()
  (text '(("かべにもじがほられている。")
          ("かうとこないし、"
           "かわないとくるんだよなぁ〜"))
        'move-mode))

(defun init-dungeon ()
  (setf *dungeon*
        (list
         (make-array (list 5 5)
                     :initial-contents
                     (list
                      (list
                       ;; 0 0
                       (make-cell :sides #(:wall nil nil :wall)
                                  :ceiling :stairs
                                  :on-moved 'up-stairs-0)
                       ;; 1 0
                       (make-cell :sides #(:wall nil :wall nil)
                                  :on-moved 'f1-find-trash-1)
                       ;; 2 0
                       (make-cell :sides #(nil nil nil nil))
                       ;; 3 0
                       (make-cell :sides #(:wall nil :wall nil))
                       ;; 4 0
                       (make-cell :sides #(:wall :wall :wall nil)
                                  :on-moved 'f1-message-1))
                      (list
                       ;; 0 1
                       (make-cell :sides #(nil :wall nil :wall))
                       ;; 1 1
                       (make-cell :sides #(:wall :wall nil :wall)
                                  :floor :stairs
                                  :on-moved (down-stairs #(1 1 1)))
                       ;; 2 1
                       (make-cell :sides #(nil :wall nil :wall))
                       ;; 3 1
                       (make-cell :sides #(:wall nil nil :wall))
                       ;; 4 1
                       (make-cell :sides #( :wall :wall nil nil)))
                      (list
                       ;; 0 2
                       (make-cell :sides #(nil :wall nil :wall))
                       ;; 1 2
                       (make-cell :sides #(nil :wall nil :wall))
                       ;; 2 2
                       (make-cell :sides #(nil :door nil :wall))
                       ;; 3 2
                       (make-cell :sides #(nil nil nil :door))
                       ;; 4 2
                       (make-cell :sides #(nil :wall nil nil)
                                  :on-moved 'f1-battle))
                      (list
                       ;; 0 3
                       (make-cell :sides #(nil :wall nil :wall))
                       ;; 1 3
                       (make-cell :sides #(nil :locked-door :wall :wall))
                       ;; 2 3
                       (make-cell :sides #(nil :wall nil :locked-door))
                       ;; 3 3
                       (make-cell :sides #(nil nil :wall :wall))
                       ;; 4 3
                       (make-cell :sides #(nil :wall :wall nil)))
                      (list
                       ;; 0 4
                       (make-cell :sides #(nil :door :wall :wall))
                       ;; 1 4
                       (make-cell :sides #(:wall :door :wall :door))
                       ;; 2 4
                       (make-cell :sides #(nil nil nil :wall))
                       ;; 3 4
                       (make-cell :sides #(:wall nil :wall nil))
                       ;; 4 4
                       (make-cell :sides #(:wall :wall :wall nil)
                                  :on-moved 'f1-find-key-1))))
         (make-array (list 5 5)
                     :initial-contents
                     (list
                      (list
                       ;; 0 0
                       (make-cell :sides #(:wall nil nil :wall))
                       ;; 1 0
                       (make-cell :sides #(:wall nil :wall nil))
                       ;; 2 0
                       (make-cell :sides #(:wall nil :wall nil))
                       ;; 3 0
                       (make-cell :sides #(:wall nil :wall nil))
                       ;; 4 0
                       (make-cell :sides #(:wall :wall nil nil)))
                      (list
                       ;; 0 1
                       (make-cell :sides #(nil :wall nil :wall))
                       ;; 1 1
                       (make-cell :sides #(:wall nil nil :wall)
                                  :ceiling :stairs
                                  :on-moved (up-stairs #(1 1 0)))
                       ;; 2 1
                       (make-cell :sides #(:wall nil :wall nil))
                       ;; 3 1
                       (make-cell :sides #(:wall :wall nil nil))
                       ;; 4 1
                       (make-cell :sides #(nil :wall nil :wall)))
                      (list
                       ;; 0 2
                       (make-cell :sides #(nil :wall nil :wall))
                       ;; 1 2
                       (make-cell :sides #(nil :wall nil :wall))
                       ;; 2 2
                       (make-cell :sides #(:wall nil nil :wall))
                       ;; 3 2
                       (make-cell :sides #(nil :wall :wall nil))
                       ;; 4 2
                       (make-cell :sides #(nil :wall nil :wall)))
                      (list
                       ;; 0 3
                       (make-cell :sides #(nil :wall nil :wall))
                       ;; 1 3
                       (make-cell :sides #(nil :wall :door :wall))
                       ;; 2 3
                       (make-cell :sides #(nil :wall :door :wall))
                       ;; 3 3
                       (make-cell :sides #(:wall :wall :door :wall)
                                  :floor :stairs
                                  :on-moved (down-stairs #(3 3 2)))
                       ;; 4 3
                       (make-cell :sides #(nil :wall nil :wall)))
                      (list
                       ;; 0 4
                       (make-cell :sides #(nil :door :wall :wall))
                       ;; 1 4
                       (make-cell :sides #(:door :door :wall :door))
                       ;; 2 4
                       (make-cell :sides #(:door :wall :wall :wall)
                                  :on-moved 'f2-message-1)
                       ;; 3 4
                       (make-cell :sides #(:door nil :wall :door))
                       ;; 4 4
                       (make-cell :sides #(nil :wall :wall nil)))))
         (make-array '(5 5) :initial-contents
                     (list
                      (list
                       ;; 0 0
                       (make-cell :sides (vector :wall nil nil :wall))
                       ;; 1 0
                       (make-cell :sides (vector :wall nil :locked-door nil))
                       ;; 2 0
                       (make-cell :sides (vector :wall nil :wall nil))
                       ;; 3 0
                       (make-cell :sides (vector :wall nil :wall nil))
                       ;; 4 0
                       (make-cell :sides (vector :wall :wall nil nil)))
                      (list
                       ;; 0 1
                       (make-cell :sides (vector nil :wall nil :wall))
                       ;; 1 1
                       (make-cell :sides (vector :locked-door nil nil nil)
                                  :on-moved 'f3-clear-item)
                       ;; 2 1
                       (make-cell :sides (vector :wall nil nil :wall))
                       ;; 3 1
                       (make-cell :sides (vector :wall :wall nil nil))
                       ;; 4 1
                       (make-cell :sides (vector nil :wall nil :wall)))
                      (list
                       ;; 0 2
                       (make-cell :sides (vector nil :wall nil :wall))
                       ;; 1 2
                       (make-cell :sides (vector :wall nil :wall :wall)
                                  :on-moved 'f3-find-key-2)
                       ;; 2 2
                       (make-cell :sides (vector nil nil nil nil))
                       ;; 3 2
                       (make-cell :sides (vector nil nil :wall nil))
                       ;; 4 2
                       (make-cell :sides (vector nil :wall nil nil)))
                      (list
                       ;; 0 3
                       (make-cell :sides (vector nil nil :wall :wall))
                       ;; 1 3
                       (make-cell :sides (vector :wall nil nil nil))
                       ;; 2 3
                       (make-cell :sides (vector nil nil nil nil))
                       ;; 3 3
                       (make-cell :sides (vector :wall :wall nil nil)
                                  :ceiling :stairs
                                  :on-moved (up-stairs #(3 3 1)))
                       ;; 4 3
                       (make-cell :sides (vector nil :wall nil :wall)))
                      (list
                       ;; 0 4
                       (make-cell :sides (vector nil nil :wall nil))
                       ;; 1 4
                       (make-cell :sides (vector nil :wall :wall nil))
                       ;; 2 4
                       (make-cell :sides (vector nil nil :wall :wall))
                       ;; 3 4
                       (make-cell :sides (vector nil nil :wall nil))
                       ;; 4 4
                       (make-cell :sides (vector nil nil :wall nil))))))))
