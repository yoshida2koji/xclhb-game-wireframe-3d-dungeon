(in-package :xclhb-game-wireframe-3d-dungeon)

(defun init-item-master ()
  (setf *item-master*
        (list :key-1 (list :name "かぎ1"
                           :on-use (lambda () (unlock-door-at-point 1 3 1 2 3 3 0)))
              :key-2 (list :name "かぎ2"
                           :on-use (lambda () (unlock-door-at-point 1 0 2 1 1 0 2)))
              :trash-1 (list :name "ごみ1"
                             :on-use (lambda ()
                                       (text '(("しかし、なにもおこらなかった..."))
                                             'select-items)))
              :clear-item (list :name "ワンダースワン"
                                :on-use (lambda ()
                                          (text '(("キュィーーン!!")
                                                  ("しかし、がめんはしろいままだ..."))
                                                'select-items))))))
