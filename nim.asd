;;;; nim.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:nim
  :description "Nim is a collection of simple number games."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:alexandria
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "game")
               (:file "nim")
               (:file "lasker")))

