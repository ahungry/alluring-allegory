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

;;;; bg-layer.lisp

(in-package :cl-user)
(defpackage alluring-allegory.bg-layer
  (:use :cl
        :glyphs)
  (:export :BG-Layer))
(in-package :alluring-allegory.bg-layer)

;;; "bg-layer" goes here. Hacks and glory await!

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
