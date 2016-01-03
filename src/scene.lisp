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
        :alluring-allegory.bg-layer
        :alluring-allegory.actor
        :glyphs)
  (:export :Scene))
(in-package :alluring-allegory.scene)

;;; "scene" goes here. Hacks and glory await!

(defclass Scene ()
  ((Title
    :accessor Title
    :initarg :title
    :initform "title")
   (Choices
    :accessor Choices
    :initarg :choices
    :initform (vector (make-instance 'Choice)))
   (Actors
    :accessor Actors
    :initarg :actors
    :initform (vector (make-instance 'Actor)))
   (Background
    :accessor Background
    :initarg :bg
    :initform (make-instance 'BG-Layer))))
