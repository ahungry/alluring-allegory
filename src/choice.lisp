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

;;;; choice.lisp

(in-package :cl-user)
(defpackage alluring-allegory.choice
  (:use :cl
        :glyphs)
  (:export :Choice :Next-Scene :Choice-Text))
(in-package :alluring-allegory.choice)

;;; "choice" goes here. Hacks and glory await!

(defclass Choice ()
  ((Next-Scene
    :accessor Next-Scene
    :initarg :next-scene
    :initform "")
   (Choice-Text
    :accessor Choice-Text
    :initarg :text
    :initform "")))
