(in-package :xclhb-game-wireframe-3d-dungeon)

(def *use-chars* (concatenate
                  'string
                  "あいうえお"
                  "かきくけこ"
                  "さしすせそ"
                  "たちつてと"
                  "なにぬねの"
                  "はひふへほ"
                  "まみむめも"
                  "やゆよ"
                  "らりるれろ"
                  "わをん"
                  "ぁぃぅぇぉ"
                  "ゃゅょっ"
                  "がぎぐげご"
                  "ざじずぜぞ"
                  "だぢづでど"
                  "ばびぶべぼ"
                  "ぱぴぷぺぽ"
                  "アイウエオ"
                  "カキクケコ"
                  "サシスセソ"
                  "タチツテト"
                  "ナニヌネノ"
                  "ハヒフヘホ"
                  "マミムメモ"
                  "ヤユヨ"
                  "ラリルレロ"
                  "ワヲン"
                  "ァィゥェォ"
                  "ャュョッ"
                  "ガギグゲゴ"
                  "ザジズゼゾ"
                  "ダヂヅデド"
                  "バビブベボ"
                  "パピプペポ"
                  "abcdefghijklmnopqrstuvwxyz"
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  "1234567890"
                  "!\"#$%&'()-^\\@[;:],./=~|`{+*}<>?_ "
                  ))

(def *convert-chars-from* "！”＃＄％＆’（）ー＾￥＠［；：］，．／＝〜｜｀｛＋＊｝＜＞？＿　")

(def *convert-chars-to* "!\"#$%&'()-^\\@[;:],./=~|`{+*}<>?_ ")

(def *glyph-width* 16)

(def *glyph-height* 16)

(defun make-glyph-offset-table (use-chars)
  (let ((table (make-hash-table :size (length use-chars))))
    (loop for ch across use-chars
          for offset from 0 by *glyph-width*
          do (setf (gethash ch table) offset))
    (loop for ch-from across *convert-chars-from*
          for ch-to across *convert-chars-to*
          do (setf (gethash ch-from table) (gethash ch-to table)))
    table))

(def *glyph-offset-table* (make-glyph-offset-table *use-chars*))

(def *image-pixmap*)

(def *image-width* 1)

(def *image-height* 1)

(def *caret-x* 0)

(def *caret-y* 0)

(def *glyph-space-x* 0)

(def *glyph-space-y* 5)

(defun update-window (&optional e)
  (declare (ignore e))
  (x:copy-area *client* *main-pixmap* *window* *gc* 0 0 0 0 *window-width* *window-height*)
  (flush))

(defun put-image (client drawable gc width height dst-x dst-y data)
  (let ((dh (floor 65535 width)))
    (multiple-value-bind (n m) (floor height dh)
      (loop for i from 0 below n
	        for y from dst-y by dh
	        do (x:put-image client x:+image-format--zpixmap+ drawable gc
			                width dh dst-x y 0 24
			                (make-array (* width dh 4)
					                    :element-type '(unsigned-byte 8)
					                    :displaced-to data
					                    :displaced-index-offset (* i dh width 4))))
      (when (> m 0)
	    (x:put-image client x:+image-format--zpixmap+ drawable gc
		             width m dst-x (- (+ dst-y height) m) 0 24
		             (make-array (* width m 4)
				                 :element-type '(unsigned-byte 8)
				                 :displaced-to data
				                 :displaced-index-offset (* (- height m) width 4)))))))

(defun convert-image (org-image)
  (destructuring-bind (h w _) (array-dimensions org-image)
    (declare (ignore _))
    (let ((image (make-array (* h w 4) :element-type (array-element-type org-image))))
      (dotimes (i h)
        (dotimes (j w)
          (let ((n (* 4 (+ (* i w) j))))
            (setf (aref image n) (aref org-image i j 2)
                  (aref image (+ n 1)) (aref org-image i j 1)
                  (aref image (+ n 2)) (aref org-image i j 0)))))
      image)))

(defun %load-image (pixmap path)
  (when pixmap
    (x:free-pixmap *client* pixmap))
  (setf pixmap (x:allocate-resource-id *client*))
  (let ((image (opticl:read-image-file path)))
    (destructuring-bind (h w ch) (array-dimensions image)
      (declare (ignore ch))
      (x:create-pixmap *client* (x:screen-root-depth *screen*) pixmap *window* w h)
      (put-image *client* pixmap *gc* w h 0 0 (convert-image image))
      (flush)
      (values pixmap w h))))

(defun load-font (path)
  (setf *font-pixmap* (%load-image *font-pixmap* path)))

(defun load-image (path)
  (multiple-value-bind (pixmap w h) (%load-image *image-pixmap* path)
    (setf *image-pixmap* pixmap
          *image-width* w
          *image-height* h)))

(defun draw-image (x y)
  (x:copy-area *client* *image-pixmap* *main-pixmap* *gc* 0 0 x y *image-width* *image-height*))


(defun draw-image-center ()
  (draw-image (floor (- *window-width* *image-width*) 2)
              (floor (- *window-height* *image-height*) 2)))

(defun draw-char (ch x y)
  (let ((offset (gethash ch *glyph-offset-table*)))
    (when offset
      (x:copy-area *client* *font-pixmap* *main-pixmap* *gc* offset 0 x y *glyph-width* *glyph-height*))))

(defun draw-string (str)
  (loop for ch across str
        do (draw-char ch *caret-x* *caret-y*)
           (setf *caret-x* (+ *caret-x* *glyph-width* *glyph-space-x*))))

(defun newline (x)
  (setf *caret-x* x)
  (setf *caret-y* (+ *caret-y* *glyph-height* *glyph-space-y*)))

(defun draw-text (text x y)
  (setf *caret-x* x
        *caret-y* y)
  (loop for str in text
        do (draw-string str)
           (newline x)))

(defun draw-lines (points)
  (x:poly-line *client* x:+coord-mode--origin+ *main-pixmap* *gc* points))

(defun fill-polygon (points)
  (x:fill-poly *client* *main-pixmap* *gc-background* x:+poly-shape--convex+ x:+coord-mode--origin+ points))

(defun draw-rect (x y w h)
  (xclhb:poly-rectangle *client* *main-pixmap* *gc* (vector (x:make-rectangle :x x :y y :width w :height h))))

(defun fill-rect (x y w h)
  (xclhb:poly-fill-rectangle *client* *main-pixmap* *gc* (vector (x:make-rectangle :x x :y y :width w :height h))))

(defun clear-rect (x y w h)
  (xclhb:poly-fill-rectangle *client* *main-pixmap* *gc-background* (vector (x:make-rectangle :x x :y y :width w :height h))))

(defun clear ()
  (clear-rect 0 0 *window-width* *window-height*)
  (flush))


(defun draw-frame (x y w h)
  (clear-rect x y w h)
  (draw-rect x y w h))

(defun draw-text-frame (x y w h)
  (draw-frame (- x (floor *glyph-width* 2))
              (- y (floor *glyph-height* 2))
              (+ w *glyph-width*)
              (+ h *glyph-height*)))

(defun %draw-text-center (text with-frame-p)
  (let* ((h (* (length text) (+ *glyph-height* *glyph-space-y*)))
         (w (loop for line in text maximize (* (length line) (+ *glyph-width* *glyph-space-x*))))
         (start-x (floor (- *window-width* w) 2))
         (start-y (floor (- *window-height* h) 2)))
    (if with-frame-p
        (draw-text-frame start-x start-y w h)
        (clear-rect start-x start-y w h))
    (draw-text text start-x start-y)
    (update-window)))

(defun draw-text-center (text &key (with-frame-p t))
  (%draw-text-center (if (listp text) text (list text)) with-frame-p))

(defun %draw-text-top (text with-frame-p)
  (let* ((w (* (+ *glyph-width* *glyph-space-x*) 40))
         (h (* (+ *glyph-height* *glyph-space-y*) 5))
         (x (floor (- *window-width* w) 2))
         (y *glyph-height*))
    (if with-frame-p
        (draw-text-frame x y w h)
        (clear-rect x y w h))
    (draw-text-frame x y w h)
    (draw-text text x y)
    (update-window)))

(defun draw-text-top (text &key (with-frame-p t))
  (%draw-text-top (if (listp text) text (list text)) with-frame-p))

(defun set-color (color)
  (x:change-gc *client* *gc* (x:make-mask x:+gc--foreground+) 0 0 color 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (flush))

(defun set-line-width (width)
  (x:change-gc *client* *gc* (x:make-mask x:+gc--line-width+) 0 0 0 0 width 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (flush))

(defun set-line-style (style)
  (x:change-gc *client* *gc* (x:make-mask x:+gc--line-style+) 0 0 0 0 0 style 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (flush))

(defun set-fill-style (style)
  (x:change-gc *client* *gc* (x:make-mask x:+gc--fill-style+) 0 0 0 0 0 0 0 0 style 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (flush))
