(in-package :xclhb-game-wireframe-3d-dungeon)

(defun init-enemy-master ()
  (setf *enemy-master*
        (list :human-1 (list :name "ぶらさがり うんどう ちゅう の かずお"
                             :hp 30
                             :ap 10
                             :dp 4
                             :sp 8
                             :exp 100
                             :pattern '(:defend :defend :attack)
                             :image-file-name "human-1.jpg")
              :human-2 (list :name "いっぷく している たけし"
                             :hp 20
                             :ap 14
                             :dp 1
                             :sp 5
                             :exp 100
                             :pattern '(:attack :attack :defend)
                             :image-file-name "human-2.jpg")
              :human-3 (list :name "よこになっている のり"
                             :hp 50
                             :ap 10
                             :dp 3
                             :sp 4
                             :exp 150
                             :pattern '(:defend :defend :attack :attack :attack :attack)
                             :image-file-name "human-3.jpg")
              :human-4 (list :name "Deamon Devil English Teacher"
                             :hp 20
                             :ap 5
                             :dp 0
                             :sp 1
                             :exp 500
                             :pattern '(:defend :defend :attack :defend :defend)
                             :image-file-name "human-4.jpg")))

  (setf *random-encounter-enemies* '(:human-1 :human-2 :human-3)))
