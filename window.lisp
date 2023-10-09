(in-package :xclhb-game-wireframe-3d-dungeon)

(def *client*)

(def *screen*)

(def *window*)

(def *window-width* 800)

(def *window-height* 600)

(def *game-loop-p*)

(def *gc*)

(def *gc-background*)

(def *main-pixmap*)

(def *font-pixmap*)

(def *key-press-handler*)

(defun flush ()
  (x:flush *client*))

(defun discard-event ()
  (let ((handler (aref (x:client-event-handlers *client*) x:+key-press-event+)))
    (x:set-event-handler *client* x:+key-press-event+ (lambda (e) (declare (ignore e))))
    (x:process-input *client*)
    (x:set-event-handler *client* x:+key-press-event+ handler)))

(defun do-noting (&rest args)
  (declare (ignore args)))

(defun convert-key (detail state)
  (case (x:keycode->keysym *client* detail state)
    ((:return) :ok)
    ((#\space :backspace :delete :escape) :cancel)
    ((#\a #\h :left) :left)
    ((#\w #\k :up) :up)
    ((#\d #\l :right) :right)
    ((#\s #\j :down) :down)))

(defun on-key-press (e)
  (x:with-key-press-event (detail state) e
    (let ((key (convert-key detail state)))
      (when key
        (funcall *key-press-handler* key)))))

;; ウィンドウサイズを固定
;; ICCCM(Inter-Client Communication Conventions Manual)を参照
;; The type of the WM_NORMAL_HINTS property is WM_SIZE_HINTS.
;; WM_NORMAL_HINTS     40
;; WM_SIZE_HINTS       41
;;
;; プロパティの構成
;; Field	Type	Comments
;; flags	CARD32	(see the next table)
;; pad	4*CARD32	For backwards compatibility
;; min_width	INT32	If missing, assume base_width
;; min_height	INT32	If missing, assume base_height
;; max_width	INT32	 
;; max_height	INT32	 
;; width_inc	INT32	 
;; height_inc	INT32	 
;; min_aspect	(INT32,INT32)	 
;; max_aspect	(INT32,INT32)	 
;; base_width	INT32	If missing, assume min_width
;; base_height	INT32	If missing, assume min_height
;; win_gravity	INT32	If missing, assume NorthWest
;;
;; flags
;; Name	Value	Field
;; USPosition	1	User-specified x, y
;; USSize	2	User-specified width, height
;; PPosition	4	Program-specified position
;; PSize	8	Program-specified size
;; PMinSize	16	Program-specified minimum size
;; PMaxSize	32	Program-specified maximum size
;; PResizeInc	64	Program-specified resize increments
;; PAspect	128	Program-specified min and max aspect ratios
;; PBaseSize	256	Program-specified base size
;; PWinGravity	512	Program-specified window gravity
(defun fix-window-resize ()
  (let ((buf (x:make-buffer (* 4 18))))
    (x:write-card32 buf 0 (logior 16 32))
    (x:write-int32 buf (* 5 4) *window-width*)
    (x:write-int32 buf (* 6 4) *window-height*)
    (x:write-int32 buf (* 7 4) *window-width*)
    (x:write-int32 buf (* 8 4) *window-height*)
    (x:change-property *client* x:+prop-mode--replace+ *window* 40 41 32 18 buf)
    (flush)))

(defun intern-atom-sync (client atom-name)
  (let ((name (x:string->card8-vector atom-name)))
    (x:intern-atom-reply-atom (x:intern-atom-sync client 0 (length name) name))))

(defun set-on-window-close-function (client window on-window-close)
  (let* ((atom-atom (intern-atom-sync client "ATOM"))
         (wm-protocols-atom (intern-atom-sync client "WM_PROTOCOLS"))
         (wm-delete-window-atom (intern-atom-sync client "WM_DELETE_WINDOW")))
    (x:change-property client 0 window wm-protocols-atom atom-atom 32 1
                       (x:card32->card8-vector wm-delete-window-atom))
    (x:set-event-handler client x:+client-message-event+ on-window-close)))

(defun open-window (&optional font-path)
  (when *client*
    (error "client has already connected."))
  (multiple-value-bind (client err) (x:x-connect)
    (when err
      (error err))
    (setf *client* client)
    (setf *screen* (elt (x:setup-roots (x:client-server-information *client*)) 0))
    (setf *window* (x:allocate-resource-id *client*))
    (setf *main-pixmap* (x:allocate-resource-id *client*))
    (setf *gc* (x:allocate-resource-id *client*))
    (setf *gc-background* (x:allocate-resource-id *client*))
    (x:set-keycode-keysym-table *client*)
    (x:create-window *client* (x:screen-root-depth *screen*) *window* (x:screen-root *screen*)
                     0 0 *window-width* *window-height* 0 0 0
                     (x:make-mask x:+cw--back-pixel+
                                  x:+cw--event-mask+)
                     0 0 0 0 0 0 0 0 0 0 0
                     (x:make-mask x:+event-mask--exposure+
                                  x:+event-mask--key-press+)
                     0 0 0)
    (x:create-gc *client* *gc* *window*
                 (x:make-mask x:+gc--background+
                              x:+gc--foreground+)
                 0 0 #xffffff 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (x:create-gc *client* *gc-background* *window*
                 (x:make-mask x:+gc--foreground+)
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (x:create-pixmap *client* (x:screen-root-depth *screen*) *main-pixmap* *window* *window-width* *window-height*)
    (x:map-window *client* *window*)
    (load-font (or font-path (merge-pathnames "resources/font.png" (uiop:getcwd))))
    (fix-window-resize)
    (set-on-window-close-function *client* *window*
                                  (lambda (e)
                                    (declare (ignore e))
                                    (setf *game-loop-p* nil)))
    (x:set-event-handler *client* x:+expose-event+ 'update-window)
    (x:set-event-handler *client* x:+key-press-event+ 'on-key-press)
    (x:set-default-error-handler *client* (lambda (e) (print e)))
    (flush)))

(defun close-window ()
  (x:x-close *client*)
  (setf *client* nil
        *game-loop-p* nil))

(defun set-window-size (w h)
  (when (or (/= w *window-width*) (/= h *window-height*))
    (setf *window-width* w
          *window-height* h)
    (fix-window-resize)
    (x:configure-window *client* *window* (x:make-mask x:+config-window--width+ x:+config-window--height+) 0 0 w h 0 0 0)
    (x:free-pixmap *client* *main-pixmap*)
    (x:create-pixmap *client* (x:screen-root-depth *screen*) *main-pixmap* *window* w h)
    (flush)))
