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

;;;; story.lisp

(in-package :cl-user)
(defpackage alluring-allegory.story
  (:use :cl
        :alluring-allegory.scene
        :alluring-allegory.choice
        :alluring-allegory.actor
        :alluring-allegory.bg-layer
        :glyphs)
  (:export :*scene-data* :scene-data-populate))
(in-package :alluring-allegory.story)

;;; "story" goes here. Hacks and glory await!

;; This is where we will define the story

(defparameter *scene-data* (make-hash-table :test #'equal))

(defparameter *scene-data-list* '())

(defun scene-data-populate ()
  "Build a list, then convert to a hash."
  (setf *scene-data-list* '())
  (push (make-instance
         'Scene
         :title "Introduction"
         :text "Welcome to the story, we hope you'll enjoy your time with us!
Are you ready to get going?"
         :choices (vector
                   (make-instance 'Choice :text "Yes" :next-scene "Onward")
                   (make-instance 'Choice :text "No" :next-scene "Comfort")
                   (make-instance 'Choice :text "Maybe" :next-scene "Onward")
                   (make-instance 'Choice :text "So" :next-scene "Onward")
                   )
         ) *scene-data-list*)

  (push (make-instance
         'Scene
         :title "Comfort"
         :text "It'll be ok, you probably didn't need it anyways."
         :choices (vector
                   (make-instance 'Choice :text "Yes" :next-scene "Comfort")
                   (make-instance 'Choice :text "No" :next-scene "Finish")
                   )
         ) *scene-data-list*)

  (push (make-instance
         'Scene
         :title "Onward"
         :text "Here we go, on a magical adventure.  Did you forget anything?"
         :choices (vector
                   (make-instance 'Choice :text "Yes" :next-scene "Comfort")
                   (make-instance 'Choice :text "No" :next-scene "Finish")
                   )
         ) *scene-data-list*)

  (push (make-instance
         'Scene
         :title "Finish"
         :text "So, what did you think of the game?"
         :choices (vector
                   (make-instance 'Choice :text "It was great!" :next-scene "bye")
                   (make-instance 'Choice :text "You suck!" :next-scene "nou")
                   )
         ) *scene-data-list*)

  (push (make-instance
         'Scene
         :title "nou"
         :text "So mean!"
         :choices (vector
                   )
         ) *scene-data-list*)

  (push (make-instance
         'Scene
         :title "bye"
         :text "Look for new updates soon!"
         :choices (vector
                   )
         ) *scene-data-list*)

  (loop for scene in *scene-data-list*
     do (setf (gethash (Title scene) *scene-data*) scene)))
