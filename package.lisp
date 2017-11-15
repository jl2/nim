;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:nim
  (:use #:cl #:alexandria)

  (:export #:*output-stream*
           #:show
           #:play

           #:nim-game
           #:human-nim-player
           #:random-nim-player
           #:smart-nim-player))

