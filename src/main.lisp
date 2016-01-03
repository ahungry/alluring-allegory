;; Alluring Allegory - A story driven game
;; Copyright (C) 2015 Matthew Carter
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; alluring-allegory.lisp

(in-package :cl-user)
(defpackage alluring-allegory
  (:use :cl
        :sdl
        :bordeaux-threads
        :glyphs
        :alluring-allegory.actor
        :glyphs)
  (:export :main))
(in-package :alluring-allegory)

;;; "alluring-allegory" goes here. Hacks and glory await!

;; Tutorials/docs here: http://code.google.com/p/lispbuilder/wiki/UsingLispbuilderSDL#The_Game_Loop
;; Standalone (eventually) here: http://code.google.com/p/lispbuilder/wiki/StandAloneExecutables

(in-readtable glyphs:syntax)

(defparameter *asset-path* (format nil "~a~a" (user-homedir-pathname) "src/lisp/alluring-allegory"))

(defmacro restartable (&body body)
  "Helper macro since we use continue restarts a lot
[remember to hit C in slime or pick the restart so errors don't kill the app]"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun sdl-font-to-texture (width height words-list &key (bpp 32) (color sdl:*white*))
  "Write a font to a texture"
  (let* ((texture (car (gl:gen-textures 1)))
         (image (sdl:create-surface (* 2 width) (* 2 height)
                                    :pixel-alpha t
                                    :bpp bpp)))
    (unless (sdl:initialise-default-font sdl:*ttf-font-vera*)
      (error "Failed to load font"))
    (sdl-ttf:init-ttf)
    (loop for line in words-list
       for y from 0 by 32
       do (when (> (length line) 0)
            (sdl:draw-string-solid-* line 0 y :surface image :color color)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :generate-mipmap t)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (sdl-base::with-pixel (pix (sdl:fp image))
      ;; Only 24 or 32 bit images work, if this errors out convert it
      (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
                              (1 :luminance)
                              (2 :luminance-alpha)
                              (3 :rgb)
                              (4 :rgba))))
        (assert (and (= (sdl-base::pixel-pitch pix)
                        (* (sdl:width image) (sdl-base::pixel-bpp pix)))
                     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
        (gl:tex-image-2d :texture-2d 0 :rgba
                         (sdl:width image) (sdl:height image)
                         0
                         texture-format
                         :unsigned-byte (sdl-base::pixel-data pix))
        ))
    texture))

(defun load-a-texture (filename)
  (let* ((texture (car (gl:gen-textures 1)))
         (image (sdl-image:load-image filename)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :generate-mipmap t)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (sdl-base::with-pixel (pix (sdl:fp image))
      ;; Only 24 or 32 bit images work, if this errors out convert it
      (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
                              (1 :luminance)
                              (2 :luminance-alpha)
                              (3 :rgb)
                              (4 :rgba))))
        (assert (and (= (sdl-base::pixel-pitch pix)
                        (* (sdl:width image) (sdl-base::pixel-bpp pix)))
                     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
        (gl:tex-image-2d :texture-2d 0 :rgba
                         (sdl:width image) (sdl:height image)
                         0
                         texture-format
                         :unsigned-byte (sdl-base::pixel-data pix))))
    texture))

(defun my-rectangle (&key (texcoords '(0 0 1 1)))
  (gl:with-primitive :quads
    (gl:tex-coord (elt texcoords 0) (elt texcoords 3))
    (gl:vertex -1 -1 0)
    (gl:tex-coord (elt texcoords 2) (elt texcoords 3))
    (gl:vertex 1 -1 0)
    (gl:tex-coord (elt texcoords 2) (elt texcoords 1))
    (gl:vertex 1 1 0)
    (gl:tex-coord (elt texcoords 0) (elt texcoords 1))
    (gl:vertex -1 1 0)))

(defparameter *resolution-width* 800)
(defparameter *resolution-height* 600)

(defparameter *words-texture* nil)
(defparameter *words* nil)
(defparameter *words-time* 0)

(ƒ untab ~"\\t"~ → |"  "| α → α)

(defparameter *rendered-story* nil)

(defun say-sdl (words)
  "Say something, but don't do this more than once a second"
  (unless (and (eq *max-renders-last* *max-renders*)
               (or (equal *words* words)
                   (> 1 (- (get-universal-time) *words-time*))))
    (let ((w *resolution-width*)
          (h (* 1 *resolution-height*)))
      ;; Break the vertical textures up into many textures based on how many lines per will fit
      (let ((max-lines-per-texture (ceiling (/ (- h 10) 16)))
            (lines (split-sequence:split-sequence #\Newline (untab words))))
        (unless (eq *words* words)
          (setf *rendered-story* nil))
        (loop for tx from 0 to (length lines) by max-lines-per-texture
           for tn from 0 ;; Texture number - for staggered rendering to avoid slow down
           do (progn
                (when (and (<= tn *max-renders*)
                           (>= tn (length *rendered-story*)))
                  ;;(sdl-font-to-texture w h (list words) :bpp 32 :color (color :r #xff :g #x00 :b #xaa)))
                  (push (sdl-font-to-texture w h (subseq lines tx
                                                         (min (+ tx max-lines-per-texture)
                                                              (length lines)))
                                             :bpp 32 :color (color :r #xff :g #x00 :b #xaa))
                        *rendered-story*))))))
    (setf *words* words
          *max-renders-last* *max-renders*
          *words-time* (get-universal-time))))

(defparameter *max-renders-last* 1)
(defparameter *max-renders* 1)
(defparameter *story* "Hello player,
Please use the arrows to navigate the story.")
(defparameter *scene* 0)

(defun get-choice (scene number)
  "Get the appropriate choices for the scene."
  (declare (ignore scene))
  (format nil "Choice ~a" number))

(defclass Choice ()
  ((Next-Scene-Id
    :accessor Next-Scene-Id
    :initarg :nsi
    :initform "")
   (Choice-Text
    :accessor Choice-Text
    :initarg :ct
    :initform "")))

(defclass BG-Layer ()
  ((Source-Image
    :accessor Source-Image
    :initarg :Source-image
    :initform "some.png")
   (Scale
    :accessor Scale
    :initarg :Scale
    :initform 1)
   (GL-Texture
    :accessor GL-Texture
    :initarg :GL-Texture
    :initform nil)
   (X-size
    :accessor X-size
    :initarg :X-size
    :initform 1000)
   (Y-size
    :accessor Y-size
    :initarg :Y-size
    :initform 1000)
   (Y-offset
    :accessor Y-offset
    :initarg :Y-offset
    :initform 0))
  (:documentation "A background layer for parallax scrolling"))

(defclass Scene ()
  ((Title
    :accessor Title
    :initarg :title
    :initform "title")
   (Choices
    :accessor Choices
    :initarg :choices
    :initform (loop for x from 0 to 4 collect (make-instance 'Choice)))
   (Actors
    :accessor Actors
    :initarg :actors
    :initform (loop for x from 0 to 1 collect (make-instance 'Actor)))
   (Background
    :accessor Background
    :initarg :bg
    :initform (make-instance 'BG-Layer))))

(defparameter *scene-data* (make-hash-table :test #'equal))
(defun scene-data-populate ()
  "Fill up the *scene-data* hash table with our various scenes."
  (setf (gethash "Introduction" *scene-data*)
        #("Good job!  You made your first choice.  Next up, we will begin our
story - are you ready for a wonderful adventure?"
          ("Onward" "Yes") ("Comfort" "No") ("Onward" "Maybe") ("Onward" "So")))
  (setf (gethash "Comfort" *scene-data*)
        #("It's ok, don't be afraid.  You can do this game, it isn't too difficult."
          ("Onward" "Ok....if you say so...")))
  (setf (gethash "Onward" *scene-data*)
        #("Now we go on"
          ("Finish" "Ok")))
  (setf (gethash "Finish" *scene-data*)
        #("Fin."
          ("Postmodem" "Thanks for the great game!")
          ("Postmodem nou!" "Your game sucked!")))
  (setf (gethash "Postmodem" *scene-data*)
        #("Thanks!"))
  (setf (gethash "Postmodem nou!" *scene-data*)
        #("No, you suck!"))
  )
(scene-data-populate)

(defun get-next-scene-id (scene-id choice)
  "Given a scene-id, and a choice, find what scene-id we go to next, as well
as the plain text description of what we just chose to get there."
  (let ((current-scene (gethash scene-id *scene-data*)))
    (if (array-in-bounds-p current-scene (1+ choice))
        (let ((next-scene-id (car (aref current-scene (1+ choice)))))
          (values next-scene-id (cadr (aref current-scene (1+ choice))))))))

(defun change-scene (choice &optional scene-override)
  "The chosen scene option."
  (setf *ox* .6 *oy* -1.5) ;; Reset the text position to the middle of screen
  (multiple-value-bind (scene-id last-choice-text) (get-next-scene-id *scene* choice)
    (when scene-override (setf scene-id scene-override))
    (when (gethash scene-id *scene-data*)
      (setf *scene* scene-id)
      (let ((scene (gethash scene-id *scene-data*)))
        (setf *story* (format nil "Scene: ~a~%'~a'~%~a"
                              scene-id
                              last-choice-text
                              (aref scene 0)))
        (say-sdl *story*)
        (loop
           for choice-slot from 1 to 4
           do (progn
                (input-to-texture (1- choice-slot)
                                  (if (array-in-bounds-p scene choice-slot)
                                      (cadr (aref scene choice-slot)) ""))))))))

(defun draw ()
  "Draw a frame"
  (cond ((> *py* 0) (setf *oy* (+ *oy* .1)))
        ((< *py* 0) (setf *oy* (- *oy* .1))))
  (cond ((> *px* 0) (setf *ox* (- *ox* .1)))
        ((< *px* 0) (setf *ox* (+ *ox* .1))))
  (when (> *ox* 2.2) (change-scene 2))
  (when (< *ox* -1.2) (change-scene 3))
  (when (> *oy* 1.0) (change-scene 0))
  (when (< *oy* -2.3) (change-scene 1))
  (setf *max-renders* (1+ (abs (round *oy*))))
  (gl:clear :color-buffer-bit)
  (gl:color 1 1 1)
  ;; Draw the main background
  (gl:with-pushed-matrix
    (gl:bind-texture :texture-2d *background-texture*)
    (gl:translate (* -1 (/ *ox* 8)) (* -1 (/ *oy* 64)) 0)
    (gl:scale 1.5 1.5 0)
    (my-rectangle :texcoords '(0 0 1 1)))
  ;; Draw the sprites that are talking
  (gl:with-pushed-matrix
    (gl:bind-texture :texture-2d *player-texture*)
    (gl:translate (* -1 (/ *ox* 4)) (- (* -1 (/ *oy* 32)) .25) 0)
    (gl:scale .8 1 0)
    (my-rectangle :texcoords '(0 0 1 1)))
  ;; Draw the speech bubbles
  (gl:with-pushed-matrix
    (gl:bind-texture :texture-2d *bubble-texture*)
    (gl:translate (- *ox* .4) (+ *oy* .9) 0)
    (gl:scale 1 .3 0)
    (my-rectangle :texcoords '(0 0 1 1)))
  ;; Draw the story that loops across lines
  (let ((rendered-story-clone (copy-list *rendered-story*)))
    (loop for rendered-story in (nreverse rendered-story-clone)
       for y from 0 by 2
       for mr from 0
       do (when (< mr (length *rendered-story*))
            (gl:with-pushed-matrix ;; Display the rendered story
              (gl:translate *ox* (- *oy* y) 0)
              (gl:bind-texture :texture-2d rendered-story)
              ;;(gl:tex-parameter :texture-2d :texture-min-filter :nearest)
              ;;(gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
              (gl:scale 1 1 0)
              (my-rectangle :texcoords '(0 0 1 1))))))
  (loop for choice from 0 to 3
     do
       (let* ((choice-position #((.3 .7 0) (.3 -1.1 0) (1.3 -.2 0) (-.45 -.2 0))) ;; n0s1e2w3
              (choice-rotation (caddr (aref choice-position choice)))
              (choice-x-pos (car (aref choice-position choice)))
              (choice-y-pos (cadr (aref choice-position choice))))
         (gl:with-pushed-matrix ;; Display the text being typed
           (gl:translate choice-x-pos choice-y-pos 0)
           (gl:rotate choice-rotation 0 0 1)
           (gl:bind-texture :texture-2d (aref *input-words-texture* choice))
           ;;(gl:tex-parameter :texture-2d :texture-min-filter :nearest)
           ;;(gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
           (gl:scale .5 .25 0)
           (my-rectangle :texcoords '(0 0 1 1)))))
  (gl:flush)
  (sdl:update-display))

(defun init ()
  ;;(setf *ox* .3 *oy* -1.0)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :texture-2d)
  ;;(setf *story-render-thread* nil)
  ;;(say-sdl "Welcome")
  (setf *scene* "Introduction")
  (setf *story* "Welcome")
  (change-scene 0 "Introduction"))

(defparameter *py* 0)
(defparameter *px* 0)
(defparameter *oy* .50)
(defparameter *ox* .50)

(defparameter *input-words* #(nil nil nil nil))
(defparameter *input-words-texture* #(nil nil nil nil))
(defparameter *input-words-time* #(0 0 0 0))

(defun input-to-texture (choice words)
  "Say something, but don't do this more than once a second"
  (let ((w (/ *resolution-width* 2))
        (h (/ *resolution-height* 4)))
    (setf (aref *input-words-texture* choice)
          (sdl-font-to-texture w h (list words) :bpp 32 :color (color :r #xff :g #x00 :b #xaa)))
    (setf (aref *input-words* choice) words
          (aref *input-words-time* choice) (get-universal-time))))

(defparameter *player-texture* nil)
(defparameter *bubble-texture* nil)
(defparameter *background-texture* nil)

(defun opengl-main ()
  "Test drawing with opengl"
  ;; Tutorial found at http://3bb.cc/tutorials/cl-opengl/
  (sdl:with-init ()
    (sdl:window *resolution-width* *resolution-height*
                :title-caption "Alluring Allegory"
                :flags sdl:sdl-opengl
                :opengl t
                :fps (make-instance 'sdl:fps-fixed :target-frame-rate 20))
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:enable-unicode)
    (let ((*player-texture* (load-a-texture "~/src/lisp/alluring-allegory/img/sprite/pink-hair.png"))
          (*bubble-texture* (load-a-texture "~/src/lisp/alluring-allegory/img/bg/bubble.png"))
          (*background-texture* (load-a-texture "~/src/lisp/alluring-allegory/img/bg/beach-oily.png")))
      (init)
      (sdl:with-events ()
        (:quit-event ()
                     t)
        (:key-down-event (:state state :scancode scancode :key key :mod-key mod-key :unicode unicode)
                         (declare (ignore key state scancode mod-key unicode))
                         (when (key-down-p :sdl-key-escape) (push-quit-event))
                         (when (or (key-down-p :sdl-key-down)
                                   (key-down-p :sdl-key-s)) (setf *py* -1))
                         (when (or (key-down-p :sdl-key-up)
                                   (key-down-p :sdl-key-w)) (setf *py* 1))
                         (when (or (key-down-p :sdl-key-right)
                                   (key-down-p :sdl-key-d)) (setf *px* -1))
                         (when (or (key-down-p :sdl-key-a)
                                   (key-down-p :sdl-key-left)) (setf *px* 1))
                         t)
        (:key-up-event (:state state :scancode scancode :key key :mod-key mod-key :unicode unicode)
                       (declare (ignore state scancode key mod-key unicode))
                       (unless (or (key-down-p :sdl-key-down)
                                   (key-down-p :sdl-key-s)
                                   (key-down-p :sdl-key-w)
                                   (key-down-p :sdl-key-up)) (setf *py* 0))
                       (unless (or (key-down-p :sdl-key-left)
                                   (key-down-p :sdl-key-a)
                                   (key-down-p :sdl-key-d)
                                   (key-down-p :sdl-key-right)) (setf *px* 0))
                       t)
        (:idle ()
               ;; this lets slime keep working while the main loop is running
               ;; in sbcl using the :fd-handler swank:*communication-style*
               ;; [something similar might help in other lisps]
               #+(and sbcl (not sb-thread)) (restartable
                                              (sb-sys:serve-all-events 0))
               (restartable (draw))))
      ;; Release textures
      (gl:delete-textures
       (append
        (list *player-texture* *bubble-texture* *background-texture*)
        *rendered-story*
        (loop for x across *input-words-texture* collect x)))
      ;; Release audio if it was missed in quit event
      )))

(defun main ()
  "Main eintry point."
  (opengl-main))
