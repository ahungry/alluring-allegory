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
        :alluring-allegory.base
        :alluring-allegory.actor
        :alluring-allegory.choice
        :alluring-allegory.bg-layer
        :alluring-allegory.scene
        :alluring-allegory.story
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
         (image (sdl:create-surface (* 2 width) (* 1 height)
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
      (let ((max-lines-per-texture (ceiling (/ (- h 10) 16))) ;; font size
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

(defun get-choice (scene number)
  "Get the appropriate choices for the scene."
  (declare (ignore scene))
  (format nil "Choice ~a" number))

(defun change-scene (choice &optional scene-override)
  "The chosen scene option."
  (setf *ox-dest* .3 *oy-dest* -.7 *pan-x-p* t *pan-y-p* t) ;; Reset the text position to the middle of screen
  (multiple-value-bind (scene-id)
      (get-next-scene-id (Current-Scene *story-singleton*) choice)
    (when scene-override (setf scene-id scene-override))
    (when (gethash scene-id (Scene-Data *story-singleton*))
      (Update-Current-Scene *story-singleton* scene-id)
      (let ((scene (Current-Scene *story-singleton*)))
        ;; TODO - Add the background change into the Story class
        (unless (equal *player-texture-source* (Full-Source-Image (aref (Actors scene) 0) *asset-path*))
          (setf *player-texture-source* (Full-Source-Image (aref (Actors scene) 0) *asset-path*))
          (setf *player-texture* (load-a-texture *player-texture-source*)))
        ;; TODO - Add the background change into the Story class
        (unless (equal *background-texture-source* (Full-Source-Image (Background scene) *asset-path*))
          (setf *background-texture-source* (Full-Source-Image (Background scene) *asset-path*))
          (setf *background-texture* (load-a-texture *background-texture-source*)))
        (say-sdl (Text scene))
        (loop
           for choice-slot from 0 to 3
           do (progn
                (input-to-texture choice-slot
                                  (if (array-in-bounds-p (Choices scene) choice-slot)
                                      (Text (aref (Choices scene) choice-slot)) ""))))))))

(defparameter *ox-dest* .3)
(defparameter *oy-dest* -.7)
(defparameter *pan-x-p* nil)
(defparameter *pan-y-p* nil)

(defun draw ()
  "Draw a frame"
  (if (or *pan-x-p* *pan-y-p*)
      ;; Auto pan the camera over
      (progn
        (cond ((< (+ *oy* .1) *oy-dest*) (setf *oy* (+ *oy* .1)))
              ((> (- *oy* .1) *oy-dest*) (setf *oy* (- *oy* .1)))
              (t (setf *pan-y-p* nil)))
        (cond ((< (+ *ox* .1) *ox-dest*) (setf *ox* (+ *ox* .1)))
              ((> (- *ox* .1) *ox-dest*) (setf *ox* (- *ox* .1)))
              (t (setf *pan-x-p* nil))))
      ;; Else, navigate according to the player input
      (progn
        (cond ((> *py* 0) (setf *oy* (+ *oy* .1)))
              ((< *py* 0) (setf *oy* (- *oy* .1))))
        (cond ((> *px* 0) (setf *ox* (- *ox* .1)))
              ((< *px* 0) (setf *ox* (+ *ox* .1)))))
      )
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
    (gl:scale 1.2 .8 0) ;; Weird perspective shift
    (my-rectangle :texcoords '(0 0 1 1)))
  ;; Draw the sprites that are talking
  (gl:with-pushed-matrix
    (gl:bind-texture :texture-2d *player-texture*)
    (gl:translate (- (* -1 (/ *ox* 4)) .2) (- (* -1 (/ *oy* 32)) .15) 0)
    (gl:scale .8 1 0)
    (my-rectangle :texcoords '(0 0 1 1)))
  ;; Draw the speech bubbles
  (when (Show-Bubble-P (Current-Scene *story-singleton*))
    (gl:with-pushed-matrix
      (gl:bind-texture :texture-2d *bubble-texture*)
      (gl:translate (- *ox* .2) (+ *oy* .3) 0)
      (gl:scale 1 .3 0)
      (my-rectangle :texcoords '(0 0 1 1))))
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
              (gl:scale .8 .4 0)
              (my-rectangle :texcoords '(0 0 1 1))))))
  (loop for choice from 0 to 3
     do
       (let* ((choice-position #((.3 .8 0) (.3 -1.1 0) (1.4 -.2 0) (-.35 -.2 0))) ;; n0s1e2w3
              (choice-rotation (caddr (aref choice-position choice)))
              (choice-x-pos (car (aref choice-position choice)))
              (choice-y-pos (cadr (aref choice-position choice))))
         (gl:with-pushed-matrix ;; Display the text being typed
           (gl:translate choice-x-pos choice-y-pos 0)
           (gl:rotate choice-rotation 0 0 1)
           (gl:bind-texture :texture-2d (aref *input-words-texture* choice))
           ;;(gl:tex-parameter :texture-2d :texture-min-filter :nearest)
           ;;(gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
           (gl:scale .6 .20 0)
           (my-rectangle :texcoords '(0 0 1 1)))))
  (gl:flush)
  (sdl:update-display))

;; Begin an instance of the story
(defparameter *story-singleton* (make-instance 'Story))

(defun init ()
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :texture-2d)
  (setf *story-singleton* (make-instance 'Story))
  (scene-data-populate *story-singleton*)
  (change-scene 0 "Prologue"))

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
(defparameter *player-texture-source* nil)
(defparameter *bubble-texture* nil)
(defparameter *background-texture* nil)
(defparameter *background-texture-source* nil)

(defun opengl-main ()
  "Test drawing with opengl"
  ;; Tutorial found at http://3bb.cc/tutorials/cl-opengl/
  (sdl:with-init ()
    (sdl:window *resolution-width* *resolution-height*
                :title-caption "Alluring Allegory"
                :flags sdl:sdl-opengl
                :opengl t
                :fps (make-instance 'sdl:fps-fixed :target-frame-rate 20))
    (sdl-mixer:OPEN-AUDIO)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:enable-unicode)
    (let ((*player-texture* (load-a-texture "~/src/lisp/alluring-allegory/img/sprite/pink-hair-gs.png"))
          (*bubble-texture* (load-a-texture "~/src/lisp/alluring-allegory/img/bg/bubble.png"))
          (*background-texture* (load-a-texture "~/src/lisp/alluring-allegory/img/bg/beach-sketch.png"))
          (music (sdl-mixer:load-music (format nil "~a/audio/~a" *asset-path* "bg-theme.mp3"))))
      (init)
      (sdl-mixer:play-music music :loop t)
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
      (sdl-mixer:halt-music)
      (sdl-mixer:free music)
      (sdl-mixer:close-audio)
      )))

(defun main ()
  "Main eintry point."
  (opengl-main))
