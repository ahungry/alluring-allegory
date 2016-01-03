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

;;;; alluring-allegory.asd

(in-package :cl-user)
(defpackage alluring-allegory-asd
  (:use :cl :asdf))
(in-package :alluring-allegory-asd)

(defsystem com.ahungry
  :version "0.1"
  :author "Matthew Carter <m@ahungry.com>"
  :license "AGPLv3"
  :depends-on (:lispbuilder-sdl
               :lispbuilder-sdl-gfx
               :lispbuilder-sdl-image
               :lispbuilder-sdl-mixer
               :lispbuilder-sdl-ttf
               :cl-opengl
               :vecto
               :drakma
               :split-sequence
               :bordeaux-threads
               :glyphs)
  :serial t ;; May not be needed
  :components ((:module "src"
                :components
                ((:file "alluring-allegory" :depends-on ("actor"))
                 (:file "aa.actor" :depends-on ("view" "model" "javascript")))))
  :description "A story driven game"
  :in-order-to ((test-op (load-op alluring-allegory-test))))
