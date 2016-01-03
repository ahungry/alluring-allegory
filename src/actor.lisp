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
(defpackage alluring-allegory.actor
  (:use :cl
        :glyphs)
  (:export :Actor))
(in-package :alluring-allegory.actor)

;;; "actor" goes here. Hacks and glory await!

(defclass Actor ()
  ((Source-Image
    :accessor Source-Image
    :initarg :si
    :initform "pink-hair.png")
   (Name
    :accessor Name
    :initarg :name
    :initform "Pink")))
