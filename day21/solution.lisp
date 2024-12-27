>;;;; 2d matrix stuff
(defun rows (m) (array-dimension m 0))
(defun cols (m) (array-dimension m 1))
(defun row (pos) (car pos))
(defun col (pos) (cdr pos))
(defun make-pos (row col) (cons row col))
(defun apply-dir (pos dir) (cons (+ (car pos) (car dir)) (+ (cdr pos) (cdr dir))))
(defvar *up* (cons -1 0))
(defvar *left* (cons 0 -1))
(defvar *right* (cons 0 1))
(defvar *down* (cons 1 0))
(defun up (pos) (apply-dir pos *up*))
(defun down (pos) (apply-dir pos *down*))
(defun left (pos) (apply-dir pos *left*))
(defun right (pos) (apply-dir pos *right*))
(defun mval (m pos) (aref m (car pos) (cdr pos)))
(defun within-bounds (m pos)
  (and (>= (row pos) 0) (>= (col pos) 0)
       (< (row pos) (rows m)) (< (col pos) (cols m))))
(defun adjs (m pos)
  (loop for adj in (list (up pos) (down pos) (left pos) (right pos))
        when (within-bounds m adj) collect adj))
;;;; end 2d matrix stuff

;;; pad keys
(defvar *dirpad-near*)
(defvar *dirkey-to-cell*)

(defun init-dirpad ()
  (setf *dirpad-near* (make-array '(2 3) :initial-element nil))
  (setf *dirkey-to-cell* (make-hash-table))
  (loop for row in '((nil :up :a) (:left :down :right))
        for r = 0 then (1+ r)
        do (loop for key in row
                 for c = 0 then (1+ c)
                 do (progn
                      (setf (aref *dirpad-near* r c) key)
                      (setf (gethash key *dirkey-to-cell*) (cons r c))
                      ))))

(defun dirkey-to-dir (key)
  (ecase key (:up *up*) (:down *down*) (:left *left*) (:right *right*)))


(defun valid-in-dirpad (pos)
  (and (within-bounds *dirpad-near* pos)
       (aref *dirpad-near* (car pos) (cdr pos))))

(init-dirpad)

(defun dirkey-to-cell (key)
  (gethash key *dirkey-to-cell*))


(defun cell-to-dirkey (cell)
  (aref *dirpad-near* (car cell) (cdr cell)))

(defun dir-close (key)
  "keys next to key in the directional pad"
  (remove nil (mapcar (lambda (cell) (aref *dirpad-near* (row cell) (col cell)))
                      (adjs *dirpad-near* (dirkey-to-cell key)))))


(defun move-to-dir-key (move)
  (cond ((equal move (cons -1 0)) :up)
        ((equal move (cons 1 0)) :down)
        ((equal move (cons 0 1)) :right)
        ((equal move (cons 0 -1)) :left)))

(defun dir-move-to-dir-key (dirk1 dirk2)
  (let ((cell1 (dirkey-to-cell dirk1))
        (cell2 (dirkey-to-cell dirk2)))
    (move-to-dir-key (cons (- (car cell2) (car cell1)) (- (cdr cell2) (cdr cell1))))))

;;(gethash (cons :up :right) *dirpad-paths*)
;;(gethash (cons :a :8) *numpad-paths*)
(defun get-dirpad-moves (start end)
  (cond
    ((and (eq start :left) (eq end :down)) (list :right))
    ((and (eq start :left) (eq end :up)) (list :right :up))
    ((and (eq start :left) (eq end :a)) (list :right :right :up))
    ((and (eq start :left) (eq end :right)) (list :right :right))
    ((and (eq start :down) (eq end :left)) (list :left))
    ((and (eq start :down) (eq end :up)) (list :up))
    ((and (eq start :down) (eq end :a)) (list :up :right))
    ((and (eq start :down) (eq end :right)) (list :right))
    ((and (eq start :up) (eq end :down)) (list :down))
    ((and (eq start :up) (eq end :left)) (list :down :left))
    ((and (eq start :up) (eq end :right)) (list :down :right))
    ((and (eq start :up) (eq end :a)) (list :right))
    ((and (eq start :right) (eq end :down)) (list :left))
    ((and (eq start :right) (eq end :up)) (list :left :up))
    ((and (eq start :right) (eq end :a)) (list :up))
    ((and (eq start :right) (eq end :left)) (list :left :left))
    ((and (eq start :a) (eq end :down)) (list :left :down))
    ((and (eq start :a) (eq end :up)) (list :left))
    ((and (eq start :a) (eq end :left)) (list :down :left :left ))
    ((and (eq start :a) (eq end :right)) (list :down))))

(defun intermediate-n-cost (moves start n memo)
  "build the dir moves that must be pressed to produce input moves, if there is an intermediate starting in start"
  (if (gethash (list moves start n) memo)
      (gethash (list moves start n) memo)
      (setf (gethash (list moves start n) memo)
            (if (zerop n)
                (length moves)
                (loop
                  for prev = start then move
                  for move in moves
                  summing (intermediate-n-cost
                           (append (get-dirpad-moves prev move) (list :a))
                           :a (1- n) memo))))))

(defun solution (dirkeys n)
  (let ((memo (make-hash-table :test #'equal)))
    (intermediate-n-cost  dirkeys :a n memo)))


(defparameter *dirs964a* (list :up :up :up :a :down :a :left :left :a :right :right :down :down :a))
(defparameter *dirs246a* (list :left :up :a :left :up :a :right :right :a :down :down :a))
(defparameter *dirs973a* (list :up :up :up :a :left :left :a :down :down :right :right  :a :down :a))
(defparameter *dirs682a* (list :up :up :a :left :up :a :down :down :a :down :right :a))
(defparameter *dirs180a* (list  :up :left :left :a  :up :up :right :a :down :down  :down :a :right :a))


(defparameter *dirs029a* (list :left :a :up :a :up :up :right :a :down :down :down :a))
(defparameter *dirs980a* (list :up :up :up :a :left :a :down :down :down :a :right :a))
(defparameter *dirs179a* (list :up :left :left :a :up :up :a :right :right :a :down :down :down :a))
(defparameter *dirs456a* (list :up :up :left :left :a :right :a :right :a :down :down :a))
(defparameter *dirs379a* (list :up :a :left :left :up :up :a :right :right :a :down :down :down :a))

;; test
(+ (* 29 (solution *dirs029a* 2))
   (* 980 (solution *dirs980a* 2))
   (* 179 (solution *dirs179a* 2 ))
   (* 456 (solution *dirs456a* 2))
   (* 379 (solution *dirs379a* 2)))

;; part1
(+ (* 964 (solution *dirs964a* 2))
   (* 246 (solution *dirs246a* 2))
   (* 973 (solution *dirs973a* 2))
   (* 682 (solution *dirs682a* 2))
   (* 180 (solution *dirs180a* 2)))


;; part2
(time  (+ (* 964 (solution *dirs964a* 25))
          (* 246 (solution *dirs246a* 25))
          (* 973 (solution *dirs973a* 25))
          (* 682 (solution *dirs682a* 25))
          (* 180 (solution *dirs180a* 25))))
