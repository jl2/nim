;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:nim
  (:use #:cl #:alexandria)
  (:export #:simple-game
           #:nim-game
           #:show
           #:play

           #:human-nim-player
           #:computer-nim-player))

