;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:nim
  (:use #:cl #:alexandria)

  (:export #:*output-stream*
           #:show
           #:play

           #:nim-game
           #:play-nim
           #:human-nim-player
           #:random-nim-player
           #:smart-nim-player

           #:lasker-nim-game
           #:play-lasker-nim
           #:human-lasker-nim-player
           #:random-lasker-nim-player
           #:smart-lasker-nim-player))

