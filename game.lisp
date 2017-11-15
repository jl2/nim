;;;; game.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:nim)

(defparameter *output-stream* t)

(defclass player ()
  ((name :initarg :name)))

(defclass game ()
  ((players :initarg :players :initform nil)))

(defgeneric show-game (game))
(defgeneric game-over-p (game))
(defgeneric take-turn (game player))
(defgeneric choose-move (game player))
(defgeneric play (game))
