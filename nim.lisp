;;;; nim.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:nim)

(defclass player ()
  ((name :initarg :name)))

(defclass simple-game ()
  ((players :initarg :players :initform nil)))

(defgeneric show-game (game))
(defgeneric winp (game))
(defgeneric take-turn (game player))
(defgeneric play (game))


(defclass human-nim-player (player)
  ((name :initform "human")))

(defclass random-nim-player (player)
  ((name :initform "random")))

(defclass smart-nim-player (player)
  ((name :initform "random")))

(defclass nim-game (simple-game)
  ((piles :initarg :piles
          :initform (make-array 3 :initial-contents '(3 5 7)))
   (players :initarg :players
            :initform (make-array 2 :initial-contents (list (make-instance 'human-nim-player)
                                                            (make-instance 'random-nim-player))))))

(defmethod show-game ((game nim-game))
  (with-slots (piles) game
    (loop for pile from 0
       for count across piles
       do
         (format t "Pile ~2a : ~a~%" pile count))))

(defun remaining (piles)
  (loop for pile across piles summing pile))

(defmethod winp ((game nim-game))
  (with-slots (piles) game
    (= 1 (remaining piles))))

(defun max-take (piles pile)
  (let ((val (aref piles pile)))
    (if (= (remaining piles) val)
        (1- val)
        val)))

(defmethod take-turn ((game nim-game) (player random-nim-player))
  (with-slots (piles) game
    (let* ((pile (loop for pile = (random (length piles)) then (random (length piles))
                    until (>= (aref piles pile) 1)
                    finally (return pile)))
           (amount (1+ (random (- (max-take piles pile) 1)))))
      (format t "~a is taking ~a from pile ~a~%" (slot-value player 'name) amount pile)
      (decf (aref piles pile) amount))))

(defmethod take-turn ((game nim-game) (player smart-nim-player))
  (with-slots (piles) game
    (let* ((pile (loop for pile = (random (length piles)) then (random (length piles))
                    until (>= (aref piles pile) 1)
                    finally (return pile)))
           (amount (1+ (random (aref piles pile)))))
      (format t "~a is taking ~a from pile ~a~%" (slot-value player 'name) amount pile)
      (decf (aref piles pile) amount))))

(defun ask-for-number (prompt min-value max-value)
  (loop
     for user-value = nil then (read)
     until (and (integerp user-value) (<= min-value user-value max-value))
     do
       (format t "~a " prompt)
     finally (return user-value)))

(defmethod take-turn ((game nim-game) (player human-nim-player))
  (with-slots (piles) game
    (let* ((pile (loop
                    for pile = (ask-for-number "Pile?" 0 (length piles)) then (ask-for-number "Pile?" 0 (length piles))
                    until (> (aref piles pile) 0)
                    finally (return pile)))
           (amount (ask-for-number "Amount?" 1 (aref piles pile))))
      (decf (aref piles pile) amount))))

(defmethod play ((game nim-game))
  (with-slots (piles players) game
    (loop
       for turn-count from 0
       for previous-player = nil then current-player
       for current-player = (random (length players)) then (mod (1+ current-player) (length players))
       for game-over = (winp game) then (winp game)

       when game-over
       do
         (with-slots (name) (aref players previous-player)
           (format t "~%~a has won after ~a turns!~%" name turn-count)
           (show-game game))

       until game-over
       do
         (let ((cp (aref players current-player)))
           (show-game game)
           (format t "Player ~a's turn (~a)~%" current-player cp)
           (take-turn game cp))
       finally (return turn-count))))
