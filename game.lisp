(in-package :xclhb-game-wireframe-3d-dungeon)

(def *key-press-handler*)

(def *dungeon*)

(def *point*)

(def *player*)

(def *player-master*)

(def *enemy-master*)

(def *random-encounter-enemies*)

(def *item-master*)

(defun floor-rows ()
  (array-dimension *floor* 0))

(defun floor-cols ()
  (array-dimension *floor* 1))

(defun px ()
  (elt *point* 0))

(defun (setf px) (x)
  (setf (elt *point* 0) x))

(defun py ()
  (elt *point* 1))

(defun (setf py) (y)
  (setf (elt *point* 1) y))

(defun pz ()
  (elt *point* 2))

(defun (setf pz) (z)
  (setf (elt *point* 2) z))

(defun %relative-point (x y dx dy d)
  (case d
    (0 (values (mod (- y dy) (floor-rows)) (mod (+ x dx) (floor-cols))))
    (1 (values (mod (+ y dx) (floor-rows)) (mod (+ x dy) (floor-cols))))
    (2 (values (mod (+ y dy) (floor-rows)) (mod (- x dx) (floor-cols))))
    (3 (values (mod (- y dx) (floor-rows)) (mod (- x dy) (floor-cols))))))

(defun relative-point (dx dy)
  (%relative-point (px) (py) dx dy *direction*))

(defun load-floor (n)
  (let ((f (elt *dungeon* n)))
    (setf *floor* (make-array (array-dimensions f)))
    (dotimes (i (floor-rows))
      (dotimes (j (floor-cols))
        (setf (aref *floor* i j)
              (let* ((cell (aref f i j))
                     (copied-cell (copy-cell cell)))
                (setf (cell-sides copied-cell) (copy-seq (cell-sides cell)))
                copied-cell))))))

(defun text (text on-text-end)
  (flet ((draw ()
           (draw-text-top (first text))
           (setf text (rest text))))
    (draw)
    (setf *key-press-handler*
          (lambda (key)
            (declare (ignore key))
            (if text
                (draw)
                (funcall on-text-end))))
    (discard-event)))


(defun options-to-text (options)
  (loop for o in options
        collect (concatenate 'string " " (second o))))

(defun select (questions options on-select on-cancel)
  (let* ((options-text (options-to-text options))
         (text (append questions options-text))
         (selected-index 0))
    (labels ((draw ()
               (dolist (str options-text)
                 (setf (elt str 0) #\space))
               (setf (elt (elt options-text selected-index) 0) #\>)
               (draw-text-top text))
             (move (n)
               (setf selected-index (mod (+ selected-index n) (length options)))
               (draw)))
      (draw)
      (setf *key-press-handler*
            (lambda (key)
              (case key
                (:up (move -1))
                (:down (move 1))
                (:ok (funcall on-select (first (elt options selected-index)) selected-index))
                (:cancel (funcall on-cancel)))))
      (discard-event))))

(defun select-simple (questions options on-cancel)
  (select questions
          options
          (lambda (v i)
            (declare (ignore i))
            (funcall (third (assoc v options))))
          on-cancel))

(defun select-yes-no (questions yes no)
  (select-simple questions
                 `((:yes "はい" ,yes)
                   (:no "いいえ" ,no))
                 no))

(defun show-status (&optional v)
  (declare (ignore v))
  (clear)
  (text (list (list (format nil "なまえ: ~a" (getf *player* :name) )
                    (format nil "Lv~a" (getf *player* :lv))
                    (format nil "HP: ~a/~a" (getf *player* :hp) (getf (getf *player* :original) :hp))
                    (format nil "AP: ~a、 DP: ~a、 SP: ~a" (getf *player* :ap) (getf *player* :dp) (getf *player* :sp))
                    (format nil "EXP: ~a" (getf *player* :exp))))
        'menu))

(defun add-item (item-id)
  (setf (getf *player* :items)
        (append (getf *player* :items)
                (list item-id))))

(defun has-item-p (item-id)
  (member item-id (getf *player* :items)))

(defun item-name (item-id)
  (getf (getf *item-master* item-id) :name))

(defun item-options (items)
  (mapcar (lambda (item)
            (list item (getf (getf *item-master* item) :name)))
          items))

(defun use-item (item-id)
  (let* ((item (getf *item-master* item-id))
         (on-use (getf item :on-use)))
    (if on-use
        (text (list (list (format nil "~a は ~a をつかった。" (getf *player* :name) (getf item :name))))
              on-use)
        (text (list (list (format nil "~a はつかうことができない。" (getf item :name))))
              'select-items))))

(defun dispose-item (index)
  (let* ((items (getf *player* :items))
         (item-id (elt items index))
         (item (getf *item-master* item-id))
         (item-name (getf item :name)))
    (select-yes-no (list (format nil "~a をほんとうにすてていいですか？" item-name))
                   (lambda ()
                     (setf (getf *player* :items)
                           (loop for i from 0
                                 for item in items
                                 unless (= i index)
                                   collect item))
                     (text (list (list (format nil "~a は ~a をすてた。" (getf *player* :name) item-name)))
                           'select-items))
                   'select-items)))

(defun select-items ()
  (let ((items (getf *player* :items)))
    (if items
        (select ()
                (item-options items)
                (lambda (item-id index)
                  (select-simple ()
                                 `((:use "つかう" ,(lambda () (use-item item-id)))
                                   (:dispose "すてる" ,(lambda () (dispose-item index))))
                                 'select-items))
                'menu)
        (text (list (list (format nil "~a は もちものをもっていない。" (getf *player* :name))))
              'menu))))

(defun menu ()
  (clear)
  (select-simple ()
                 '((:status "じょうたい" show-status)
                   (:items "もちもの" select-items)
                   (:stop "ちゅうだんする" start-menu))
                 'move-mode))

(defun move-mode ()
  (setf *key-press-handler* 'move-mode-key-handler)
  (draw-floor)
  (discard-event))

(defun %move-forward ()
  (multiple-value-bind (y x) (relative-point 0 1)
    (setf (px) x
          (py) y)
    (draw-floor)
    (let ((on-moved (cell-on-moved (aref *floor* (py) (px)))))
      (if on-moved
          (funcall on-moved)
          (random-encounter)))))

(defun change-direction (dd)
  (setf *direction* (mod (+ *direction* dd) 4))
  (draw-floor)
  (random-encounter))

(defun move-forward (&optional open-door-p)
  (let* ((cell (aref *floor* (py) (px)))
         (side (elt (cell-sides cell) *direction*)))
    (case side
      ((:wall :locked-door) (draw-text-center '("かべだ")))
      (:door (if open-door-p
                 (%move-forward)
                 (draw-text-center '("かべだ"))))
      (otherwise (%move-forward)))))

(defun move-mode-key-handler (key)
  (case key
    (:ok (move-forward t))
    (:up (move-forward nil))
    (:left (change-direction 3))
    (:right (change-direction 1))
    (:down (change-direction 2))
    (:cancel (menu))))

(defun unlock-door (x y d)
  (setf (elt (cell-sides (aref *floor* y x)) d) :door))

(defun unlock-door-at-point (x1 y1 d1 x2 y2 d2 z)
  (cond ((or (and (equalp *point* (vector x1 y1 z)) (eql *direction* d1))
             (and (equalp *point* (vector x2 y2 z)) (eql *direction* d2)))
         (unlock-door x1 y1 d1)
         (unlock-door x2 y2 d2)
         (text '(( "かぎがあいた。"))
               'select-items))
        (t
         (text '(( "しかし、かぎがあわなかった。"))
               'select-items))))

(defun random-encounter ()
  (when (= (random 10) 0)
    (battle (random-elt *random-encounter-enemies*)
            'move-mode
            'reset-game
            'move-mode))
  )

(defun up-down-stairs (point text)
  (lambda ()
    (select-yes-no text
                   (lambda ()
                     (setf *point* point)
                     (load-floor (pz))
                     (move-mode))
                   'move-mode)))

(defun up-stairs (point)
  (up-down-stairs point '("のぼりかいだん が ある。"
                          "のぼりますか？")))

(defun down-stairs (point)
  (up-down-stairs point '("くだりかいだん が ある。"
                          "おりますか？")))

(defun up-stairs-0 ()
  (select-yes-no '("のぼりかいだん が ある。"
                   "のぼりますか？")
                 (lambda ()
                   (level-up (lambda ()
                               (reset-player-parameters)
                               (setf *point* nil)
                               (start-menu))))
                 'move-mode))

(defun level-up (next)
  (let ((org (getf *player* :original)))
    (cond ((>= (getf org :exp) (* (getf org :lv) 1000))
           (let ((hp (random+1 5))
                 (ap (random+1 3))
                 (dp (random+1 3))
                 (sp (random+1 3)))
             (incf (getf org :lv))
             (incf (getf org :hp) hp)
             (incf (getf org :ap) ap)
             (incf (getf org :dp) dp)
             (incf (getf org :sp) sp)
             (text (list (list (format nil "~a のレベルが ~a にあがった" (getf *player* :name) (getf org :lv))
                               (format nil "HPが ~a あがった" hp)
                               (format nil "APが ~a あがった" ap)
                               (format nil "DPが ~a あがった" dp)
                               (format nil "SPが ~a あがった" sp)))
                   (lambda () (level-up next)))))
          (t (funcall next)))))

(defun reset-player-parameters ()
  (copy-props (getf *player* :original) *player* :hp :ap :dp :sp :lv :exp))

(defun start-menu ()
  (clear)
  (select-simple ()
                 (list (if *point*
                           `(:restart "さいかい"
                                      ,(lambda ()
                                         (load-floor (pz))
                                         (move-mode)))
                           `(:start "めいきゅうにはいる"
                                    ,(lambda ()
                                       (setf *point* (vector 0 0 0))
                                       (load-floor 0)
                                       (move-mode))))
                       `(:reset "セーブデータをさくじょする"
                                ,(lambda ()
                                   (select-yes-no '("ほんとうにセーブデータをさくじょしても いいですか？")
                                                  (lambda ()
                                                    (init-save-data)
                                                    (text '(("セーブデータをさくじょしました。"))
                                                          'start-menu))
                                                  'start-menu)))
                       `(:exit "ゲームをしゅうりょうする"
                               ,(lambda ()
                                  (setf *game-loop-p* nil))))
                 'start-menu))

(defun load-enemy (id)
  (let ((enemy (getf *enemy-master* id)))
    (load-image (merge-pathnames (getf enemy :image-file-name) (merge-pathnames "resources/" (uiop:getcwd))))
    (copy-list enemy)))

(defun attack (from to action next on-kill)
  (let ((damage (max 0 (- (getf from :ap) (* (getf to :dp) (if (eql action :defend) 2 1))))))
    (decf (getf to :hp) damage)
    (text (list (format nil "~a の こうげき" (getf from :name))
                (format nil "~a に ~a のダメージ" (getf to :name) damage))
          (if (> (getf to :hp) 0)
              next
              on-kill))))

(defun defend (o next)
  (text (list (format nil "~a は みをまもっている" (getf o :name)))
        next))

(defun battle (enemy-id on-win on-lose on-escape)
  (let* ((enemy (load-enemy enemy-id))
         (enemy-action-pattern-i 0)
         (enemy-action-pattern (getf enemy :pattern))
         (enemy-action)
         (player-action))
    (labels ((command ()
               (select ()
                       '((:attack "こうげき")
                         (:defend "ぼうぎょ")
                         (:escape "にげる"))
                       #'process
                       #'command))
             (process (v _)
               (declare (ignore _))
               (setf enemy-action (elt enemy-action-pattern enemy-action-pattern-i))
               (setf enemy-action-pattern-i (mod (1+ enemy-action-pattern-i) (length enemy-action-pattern)))
               (case v
                 (:attack
                  (setf player-action :attack)
                  (if (> (getf *player* :sp) (getf enemy :sp))
                      (player-act (lambda () (enemy-act #'command)))
                      (enemy-act (lambda () (player-act #'command)))))
                 (:defend
                  (setf player-action :defend)
                  (enemy-act #'command))
                 (:escape
                  (text (list (format nil "~a は にげようとした。" (getf *player* :name)))
                        (if (= (random 3) 0)
                            on-escape
                            (lambda ()
                              (text '("しかし、にげられなかった。")
                                    (lambda ()
                                      (enemy-act #'command)))))))))
             (player-act (next)
               (case player-action
                 (:attack
                  (attack *player* enemy enemy-action next
                          (lambda ()
                            (text (list (format nil "~a を たおした" (getf enemy :name))
                                        (format nil "~a の けいけんちをかくとく" (getf enemy :exp)))
                                  (lambda ()
                                    (incf (getf (getf *player* :original) :exp) (getf enemy :exp))
                                    (funcall on-win))))))
                 (:defend
                  (defend *player* next))))
             (enemy-act (next)
               (case enemy-action
                 (:attack
                  (attack enemy *player* player-action next
                          (lambda ()
                            (text (list (format nil "~a は しんでしまった" (getf *player* :name)))
                                  on-lose))))
                 (:defend
                  (defend enemy next)))))
      (draw-image-center)
      (text (list (format nil "~a が あらわれた。" (getf enemy :name)))
            #'command))))

(defun start-game-loop ()
  (when *game-loop-p*
    (error "game loop has already started."))
  (setf *game-loop-p* t)
  (loop while *game-loop-p*
        do (x:process-input *client*)
           (sleep 0.01))
  (setf *game-loop-p* nil))

(defun title ()
  (clear)
  (draw-text-center '("#1 Get the WS back") :with-frame-p nil)
  (setf *key-press-handler*
        (lambda (key)
          (declare (ignore key))
          (clear)
          (start-menu)))
  (discard-event))

(defun init-save-data ()
  (setf *direction* 0)
  (setf *point* nil)
  (setf *player* (copy-seq *player-master*))
  (setf (getf *player* :original) (copy-seq *player-master*))
  (setf (getf *player* :items) ()))

(defun save-game-data ()
  (with-open-file (out (merge-pathnames "savedata" (uiop:getcwd))
                       :direction :output
                       :if-exists :supersede)
    (print (list :player *player*
                 :direction *direction*
                 :point *point*)
           out)))

(defun load-save-data ()
  (let ((path (uiop:file-exists-p (merge-pathnames "savedata" (uiop:getcwd)))))
    (if path
        (with-open-file (in path)
          (let ((savedata (read in)))
            (setf *player* (getf savedata :player)
                  *direction* (getf savedata :direction)
                  *point* (getf savedata :point))))
        (init-save-data))))

(defun reset-game ()
  (init-save-data)
  (title))

(defun start-game ()
  (load-save-data)
  (title)
  (start-game-loop)
  (save-game-data))

