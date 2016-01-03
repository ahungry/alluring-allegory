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
  (:export :Story :Last-Scene :Current-Scene :Scene-Data :Scene-Data-Populate
           :Update-Current-Scene))
(in-package :alluring-allegory.story)

;;; "story" goes here. Hacks and glory await!

;; This is where we will define the story

(defclass Story ()
  ((Title
    :accessor Story-Title
    :initarg :title
    :initform "The title of our story")
   (Last-Scene
    :accessor Last-Scene
    :initarg :last-scene
    :initform (make-instance 'Scene :title "Introduction" :text "Welcome to the story!"))
   (Current-Scene
    :accessor Current-Scene
    :initarg :current-scene
    :initform (make-instance 'Scene :title "Introduction" :text "Welcome to the story!"))
   (Scene-Data
    :accessor Scene-Data
    :initarg :scene-data
    :initform (make-hash-table :test #'equal)))
  (:documentation "the story"))

(defgeneric Update-Current-Scene (object scene-id)
  (:documentation "Change from the current scene"))

(defmethod Update-Current-Scene ((story Story) scene-id)
  (setf (Last-Scene story) (Current-Scene story))
  (setf (Current-Scene story) (gethash scene-id (Scene-Data story))))

(defgeneric Scene-Data-Populate (object)
  (:documentation "Begin the story."))

(defmethod Scene-Data-Populate ((story Story))
  "Build a list, then convert to a hash."
  (let ((scene-data-list '()))
    (push (make-instance
           'Scene
           :title "Introduction"
           :text "Welcome to the story, I hope you'll enjoy your time
with us!  To choose what to do and where to go, simply move this
bubble by using your arrow keys.

Are you ready to get going?"
           :choices (vector
                     (make-instance 'Choice :text "Yes" :next-scene "Onward")
                     (make-instance 'Choice :text "No" :next-scene "Comfort")
                     (make-instance 'Choice :text "Maybe" :next-scene "Comfort")
                     (make-instance 'Choice :text "Heck yea!" :next-scene "Onward")
                     )
           ) scene-data-list)

    (push (make-instance
           'Scene
           :title "Comfort"
           :text "Now, now, don't be afraid to get started on your journey.
It may not look like much, but I think you'll find this game worth your while.

Speaking of which, do you know what an allegory is?"
           :choices (vector
                     (make-instance 'Choice :text "Yes" :next-scene "Allegory - I know that!")
                     (make-instance 'Choice :text "No" :next-scene "Allegory?")
                     )
           ) scene-data-list)

    (push (make-instance
           'Scene
           :title "Allegory - I know that!"
           :text "Hah, cool - well, in that case, I won't bother talking about
it too much, but you may find that you can draw parallels between this and that..."
           :choices (vector
                     (make-instance 'Choice :text "Yes" :next-scene "Pinky")
                     )
           ) scene-data-list)

    (push (make-instance
           'Scene
           :title "Allegory?"
           :text "Well - it's sort of like...hmm...how would I explain it.

You know when you talk about one thing but mean another?  Yes?  No?  Well, either
way, I think that's enough about that, let's get going."
           :choices (vector
                     (make-instance 'Choice :text "Alright..." :next-scene "Pinky")
                     )
           ) scene-data-list)

    (push (make-instance
           'Scene
           :title "Pinky"
           :text "So, you want to know who I am?  Seriously?  You don't remember?
Well, no matter, let me introduce myself.

I'm the totally awesome, super cliche, trope satisfying girl named
Pinky!"
           :choices (vector
                     (make-instance 'Choice :text "Nice to meet you Pinky!" :next-scene "Onward")
                     (make-instance 'Choice :text "Oh joy..." :next-scene "Onward")
                     )
           ) scene-data-list)

    (push (make-instance
           'Scene
           :title "Onward"
           :text "Here we go, on a magical adventure!  Are you excited?

Wait - did you forget anything?  (Did we talk about much yet?  Do you
even know who I am?)"
           :choices (vector
                     (make-instance 'Choice :text "Err, no, I may have forgot to ask..." :next-scene "Comfort")
                     (make-instance 'Choice :text "Yes!  Lets get going already!" :next-scene "Finish")
                     )
           ) scene-data-list)

    (push (make-instance
           'Scene
           :title "Finish"
           :text "So, what did you think of the game?  It still needs
a lot of work, so go easy on me, I think the heart of the game is
going to be in the story and art, more than any game shattering
paradigm shifts, but that's ok!  Look for this to grow quite a bit!"
           :choices (vector
                     (make-instance 'Choice :text "It was great!" :next-scene "bye")
                     (make-instance 'Choice :text "You suck!" :next-scene "nou")
                     )
           ) scene-data-list)

    (push (make-instance
           'Scene
           :title "nou"
           :text "So mean!"
           :choices (vector
                     )
           ) scene-data-list)

    (push (make-instance
           'Scene
           :title "bye"
           :text "Look for new updates soon!"
           :choices (vector
                     )
           ) scene-data-list)

    (loop for scene in scene-data-list
       do (setf (gethash (Title scene) (Scene-data story)) scene))))
