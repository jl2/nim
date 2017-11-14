;;;; nim.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:nim)

(defclass player ()
  ((name :initarg :name)))

(defclass simple-game ()
  ((players :initarg :players :initform nil)))

(defgeneric show-game (game))
(defgeneric game-over-p (game))
(defgeneric take-turn (game player))
(defgeneric choose-move (game player))
(defgeneric play (game))

(defparameter *output-stream* t)

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
         (format *output-stream* "Pile ~2a : ~a~%" pile count))))

(defun remaining (piles)
  (loop for pile across piles summing pile))

(defmethod game-over-p ((game nim-game))
  (with-slots (piles) game
    (= 1 (remaining piles))))

(defun max-take (piles pile)
  (let ((val (aref piles pile)))
    (if (= (remaining piles) val)
        (1- val)
        val)))

(defun nim-sum (piles)
  (apply #'logxor (coerce piles 'list)))

(defun non-zero-piles (piles)
  (loop
     for idx from 0
     for pile across piles
     when (not (zerop pile))
     collecting idx))

(defmethod take-turn ((game nim-game) player)
  (with-slots (piles players) game
    (multiple-value-bind (pile amount) (choose-move game player)
      (format *output-stream* "~a is taking ~a from pile ~a~%" (slot-value player 'name) amount pile)
      (decf (aref piles pile) (max 1 amount)))))

(defmethod choose-move ((game nim-game) (player random-nim-player))
  (with-slots (piles) game
    (let* ((pile (loop for pile = (random (length piles)) then (random (length piles))
                    until (>= (aref piles pile) 1)
                    finally (return pile)))
           (mt (max-take piles pile))
           (amount (1+ (random mt))))
      (values pile amount))))

(defmethod choose-move ((game nim-game) (player smart-nim-player))
  (with-slots (piles) game
    (let* ((nzps (non-zero-piles piles))
           (ns (nim-sum piles))
           (first-idx (car nzps))
           (second-idx (cadr nzps))
           (first-pile (aref piles first-idx))
           (second-pile (aref piles second-idx)))
      (cond
        ;; If there's only one pile, take all but one
        ((= 1 (length nzps))
         (values (car nzps) (1- (aref piles (car nzps)))))

        ;; If there's two piles, and one of them is 1, take all of the other pile
        ((and (= 2 (length nzps)) (= 1 first-pile))
         (values second-idx second-pile))

        ((and (= 2 (length nzps)) (= 1 second-pile)
              (values first-idx first-pile)))

        ;; If one pile is greater than the other, make them equal
        ((and (= 2 (length nzps)) (> first-pile second-pile))
         (values first-idx (- first-pile second-pile )))

        ((= 2 (length nzps))
         (values second-idx (- second-pile first-pile)))

        ;; If the nim-sum is 0, choose at random
        ((zerop (nim-sum piles))
         (let* ((pile (loop for pile = (random (length piles)) then (random (length piles))
                         until (>= (aref piles pile) 1)
                         finally (return pile)))
                (mt (max-take piles pile))
                (amount (1+ (random mt))))
           (values pile (max 1 amount))))

        ;; Try to make the nim-sum 0
        (t
         (loop for idx from 0
            for pile across piles
            for target = (logxor pile ns) then (logxor pile ns)
            until (< target pile)
            finally (return (values idx (max 1 (if (= ns (remaining piles))
                                                   (1- (- pile target))
                                                   (- pile target)))))))))))
(defun ask-for-number (prompt min-value max-value)
  (loop
     for user-value = nil then (read)
     until (and (integerp user-value) (<= min-value user-value max-value))
     do
       (format *output-stream* "~a " prompt)
     finally (return user-value)))

(defmethod choose-move ((game nim-game) (player human-nim-player))
  (with-slots (piles) game
    (let* ((pile (loop
                    for pile = (ask-for-number "Pile?" 0 (length piles)) then (ask-for-number "Pile?" 0 (length piles))
                    until (> (aref piles pile) 0)
                    finally (return pile)))
           (amount (ask-for-number "Amount?" 1 (max-take piles pile))))
      (values pile amount))))

(defmethod play ((game nim-game))
  (with-slots (piles players) game
    (loop
       for turn-count from 0
       for previous-player = nil then current-player
       for current-player = (random (length players)) then (mod (1+ current-player) (length players))
       for game-over = (game-over-p game) then (game-over-p game)

       when game-over
       do
         (with-slots (name) (aref players previous-player)
           (format *output-stream* "~%~a has won after ~a turns!~%" name turn-count)
           (show-game game))

       until game-over
       do
         (let ((cp (aref players current-player)))
           (show-game game)
           (format *output-stream* "Player ~a's turn (~a)~%" current-player cp)
           (take-turn game cp))
       finally (return turn-count))))
