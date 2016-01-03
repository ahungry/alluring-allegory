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

(asdf:defsystem #:alluring-allegory
  :description "A story driven game"
  :author "Matthew Carter <m@ahungry.com>"
  :license "AGPLv3"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-gfx
               #:lispbuilder-sdl-image
               #:lispbuilder-sdl-mixer
               #:lispbuilder-sdl-ttf
               #:cl-opengl
               #:vecto
               #:drakma
               #:split-sequence
               #:bordeaux-threads
               #:glyphs)
  :serial t
  :components ((:file "package")
               (:file "alluring-allegory")))
