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
  (:export :scene-data-populate))
(in-package :alluring-allegory.story)

;;; "story" goes here. Hacks and glory await!

;; This is where we will define the story

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
