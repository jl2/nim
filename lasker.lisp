;;;; lasker.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:nim)

(defclass lasker-nim-player (nim-player)
  ())

(defclass human-lasker-nim-player (human-nim-player lasker-nim-player)
  ())

(defclass random-lasker-nim-player (random-nim-player lasker-nim-player)
  ())

(defclass smart-lasker-nim-player (smart-nim-player lasker-nim-player)
  ())

(defclass lasker-nim-game (nim-game)
  ((piles :initarg :piles
          :initform (make-array 3 :initial-contents '(3 5 7) :adjustable t :fill-pointer 3))
   (players :initarg :players
            :initform (make-array 2 :initial-contents (list (make-instance 'human-lasker-nim-player)
                                                            (make-instance 'random-lasker-nim-player))))))

(defun splittable-piles (piles)
  (loop
     for i from 0
     for pile across piles
     when (> pile 2) collect i))

(defmethod take-turn ((game lasker-nim-game) (player nim-player))
  (with-slots (piles players) game
    (multiple-value-bind (pile amount) (choose-move game player)
      (format *output-stream* "~a is taking ~a from pile ~a~%" (slot-value player 'name) amount pile)
      (decf (aref piles pile) (max 1 amount)))))

(defmethod take-turn ((game lasker-nim-game) (player lasker-nim-player))
  (with-slots (piles players) game
    (multiple-value-bind (action pile amount) (choose-move game player)
      (cond
        ((eq :split action)
         (format *output-stream* "~a is splitting ~a from pile ~a~%" (slot-value player 'name) amount pile)
         (decf (aref piles pile) (max 1 amount))
         (vector-push-extend amount piles))
        ((eq :take action)
         (format *output-stream* "~a is taking ~a from pile ~a~%" (slot-value player 'name) amount pile)
         (decf (aref piles pile) (max 1 amount)))
        (t
         (error "Don't know how to ~a" action))))))

(defmethod choose-move ((game lasker-nim-game) (player random-lasker-nim-player))
  (with-slots (piles) game
    (let ((splittable (splittable-piles piles)))
      (cond
        ((and splittable (> 0.5 (random 1.0)))
         ;; Split pile
         (let* ((pile (random-elt splittable))
                (ms (1- (aref piles pile)))
                (amount (if (<= 1 ms) ms (1+ (random ms)))))
           (values :split pile amount)))

        (t
         ;; Take from pile
         (let* ((pile (loop for pile = (random (length piles)) then (random (length piles))
                         until (>= (aref piles pile) 1)
                         finally (return pile)))
                (mt (max-take piles pile))
                (amount (1+ (random mt))))
           (values :take pile amount)))))))

(defmethod choose-move ((game lasker-nim-game) (player smart-lasker-nim-player))
  (with-slots (piles) game
    (let* ((nzps (non-zero-piles piles))
           (ns (nim-sum piles))
           (first-idx (car nzps))
           (second-idx (cadr nzps))
           (first-pile (aref piles first-idx))
           (second-pile (if second-idx (aref piles second-idx) nil)))
      (cond
        ;; If there's only one pile, take all but one
        ((= 1 (length nzps))
         (values :take (car nzps) (1- (aref piles (car nzps)))))

        ;; If there's two piles, and one of them is 1, take all of the other pile
        ((and (= 2 (length nzps)) (= 1 first-pile) second-pile)
         (values :take second-idx second-pile))

        ((and (= 2 (length nzps)) second-pile (= 1 second-pile))
         (values :take first-idx first-pile))

        ;; If one pile is greater than the other, make them equal
        ((and (= 2 (length nzps)) second-pile (> first-pile second-pile))
         (values :take first-idx (- first-pile second-pile )))

        ((and second-pile (= 2 (length nzps)))
         (values :take second-idx (- second-pile first-pile)))

        ;; If the nim-sum is 0, choose at random
        ((zerop (nim-sum piles))
         (let* ((pile (loop for pile = (random (length piles)) then (random (length piles))
                         until (>= (aref piles pile) 1)
                         finally (return pile)))
                (mt (max-take piles pile))
                (amount (1+ (random mt))))
           (values :take pile (max 1 amount))))

        ;; Try to make the nim-sum 0
        (t
         (loop for idx from 0
            for pile across piles
            for target = (logxor pile ns) then (logxor pile ns)
            until (< target pile)
            finally (return (values :take idx (max 1 (if (= ns (remaining piles))
                                                         (1- (- pile target))
                                                         (- pile target)))))))))))
(defun ask-for-choice (prompt choices)
  (loop
     for user-value = nil then (read)
     until (and user-value (find user-value choices))
     do
       (format t "~a~%~{~a~%~}> " prompt choices)
     finally (return user-value)))

(defmethod choose-move ((game lasker-nim-game) (player human-lasker-nim-player))
  (with-slots (piles) game
    (let* ((action (ask-for-choice "Action?" '(:take :split)))
           (pile (loop
                    for pile = (ask-for-number "Pile?" 0 (length piles)) then (ask-for-number "Pile?" 0 (length piles))
                    until (> (aref piles pile) 0)
                    finally (return pile)))
           (amount (ask-for-number "Amount?" 1 (max-take piles pile))))
      (values action pile amount))))


(defun play-lasker-nim ()
  (nim:play (make-instance
             'nim:lasker-nim-game
             :players
             (make-array 2
                         :initial-contents
                         (list
                          (make-instance 'nim:random-lasker-nim-player :name "random")
                          (make-instance 'nim:human-lasker-nim-player :name "human"))))))
