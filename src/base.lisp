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

;;;; base.lisp

(in-package :cl-user)
(defpackage alluring-allegory.base
  (:use :cl
        :glyphs)
  (:export :Base :Source-Image :Title :Text
           :Full-Source-Image))
(in-package :alluring-allegory.base)

;;; "base" goes here. Hacks and glory await!

(defclass Base ()
  ((Source-Image
    :accessor Source-Image
    :initarg :source-image
    :initform "beach.png")
   (Title
    :accessor Title
    :initarg :title
    :initform "Title")
   (Text
    :accessor Text
    :initarg :text
    :initform "")))

(defgeneric Full-Source-Image (object asset-path)
  (:documentation "Return the full object path."))

(defmethod Full-Source-Image ((base Base) asset-path)
  (format nil "~a/img/~a" asset-path (Source-Image base)))
