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

;;;; scene.lisp

(in-package :cl-user)
(defpackage alluring-allegory.scene
  (:use :cl
        :alluring-allegory.base
        :alluring-allegory.bg-layer
        :alluring-allegory.actor
        :alluring-allegory.choice
        :glyphs)
  (:export :Scene :Choices :Actors :Background :Show-Bubble-P
           :get-next-scene-id))
(in-package :alluring-allegory.scene)

;;; "scene" goes here. Hacks and glory await!

(defclass Scene (Base)
  ((Title
    :accessor Title
    :initarg :title
    :initform "title")
   (Text
    :accessor Text
    :initarg :text
    :initform "Your scene description here.")
   (Choices
    :accessor Choices
    :initarg :choices
    :initform (vector (make-instance 'Choice)))
   (Actors
    :accessor Actors
    :initarg :actors
    :initform (vector (make-instance 'Actor)))
   (Show-Bubble-P
    :accessor Show-Bubble-P
    :initarg :show-bubble-p
    :initform nil)
   (Background
    :accessor Background
    :initarg :background
    :initform (make-instance 'BG-Layer))))

(defgeneric get-next-scene-id (object choice)
  (:documentation "Blabla"))

(defmethod get-next-scene-id ((current-scene Scene) choice)
  "Given a scene-id, and a choice, find what scene-id we go to next, as well
as the plain text description of what we just chose to get there."
  (if (array-in-bounds-p (Choices current-scene) choice)
      (let ((next-scene-id (Next-Scene (aref (Choices current-scene) choice))))
        (values next-scene-id (Text (aref (Choices current-scene) choice))))))
