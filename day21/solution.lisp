;;;; 2d matrix stuff
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
(defvar *numpad-near*)
(defvar *dirkey-to-cell*)
(defvar *numkey-to-cell*)

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

(defun init-numpad ()
  (setf *numpad-near* (make-array '(4 3) :initial-element nil))
  (setf *numkey-to-cell* (make-hash-table))
  (loop for row in '((:7 :8 :9) (:4 :5 :6) (:1 :2 :3) (nil :0 :a))
        for r = 0 then (1+ r)
        do (loop for key in row
                 for c = 0 then (1+ c)
                 do (progn
                      (setf (aref *numpad-near* r c) key)
                      (setf (gethash key *numkey-to-cell*) (cons r c))
                      ))))

(defun valid-in-dirpad (pos)
  (and (within-bounds *dirpad-near* pos)
       (aref *dirpad-near* (car pos) (cdr pos))))

(defun valid-in-numpad (pos)
  (and (within-bounds *numpad-near* pos)
       (aref *numpad-near* (car pos) (cdr pos))))

(init-dirpad)
(init-numpad)

(defun dirkey-to-cell (key)
  (gethash key *dirkey-to-cell*))

(defun numkey-to-cell (key)
  (gethash key *numkey-to-cell*))

(defun cell-to-dirkey (cell)
  (aref *dirpad-near* (car cell) (cdr cell)))

(defun cell-to-numkey (cell)
  (aref *numpad-near* (car cell) (cdr cell)))

(defun dir-close (key)
  "keys next to key in the directional pad"
  (remove nil (mapcar (lambda (cell) (aref *dirpad-near* (row cell) (col cell)))
                      (adjs *dirpad-near* (dirkey-to-cell key)))))

(defun num-close (key)
  "keys next to key in the numerical pad"
  (remove nil (mapcar (lambda (cell) (aref *numpad-near* (row cell) (col cell)))
                      (adjs *numpad-near* (numkey-to-cell key)))))
;; end pad keys

(defstruct state visited unvisited)

(defun init-state (start)
  (let ((state (make-state
                :unvisited (make-hash-table)
                :visited (make-hash-table))))
    (setf (gethash start (state-unvisited state)) '())
    state))

(defun closest-unvisited (state)
  (loop
    with closest = nil
    for key being the hash-keys in (state-unvisited state) using (hash-value path)
    do (when (or (not closest)
                 (< (length path)
                    (length (gethash closest (state-unvisited state)))))
         (setf closest key))
    finally (return closest)))

(defun in-hashtable (key table)
  (nth-value 1 (gethash key table)))

(defun valid-next-keys-non-visited (key close-function visited)
  (remove-if (lambda (k) (in-hashtable k visited))
             (funcall close-function key)))


(defun dijkstra (start adjs-function)
  "return the minimum path of keys from start to the others.
notice that this is the path of KEYS and not of movements!"
  (loop
    with state = (init-state start)
    for closest = (closest-unvisited state)
    while closest
    do (let* ((adjs (valid-next-keys-non-visited closest adjs-function (state-visited state))))
         (loop for adj in adjs
               do (when (or (not (in-hashtable adj (state-unvisited state)))
                            (< (1+ (length (gethash closest (state-unvisited state))))
                               (length (gethash adj (state-unvisited state))))
                            )
                    (setf (gethash adj (state-unvisited state))
                          (append (gethash closest (state-unvisited state)) (list adj)))))
         (setf (gethash closest (state-visited state)) (gethash closest (state-unvisited state)))
         (remhash closest (state-unvisited state)))
    finally (return (state-visited state))))

;; pre-calculate moves from dirkey to dirkey and from numkey to numkey
(defvar *dirpad-paths*) ;; (dirkey . dirkey) -> dirkeys
(defvar *numpad-paths*) ;; (numkey . numkey) -> dirkeys

(defun init-dirpad-paths ()
  (setf *dirpad-paths* (make-hash-table :test #'equal))
  (loop
    for start in (list :up :down :left :right :a)
    do (loop
         with paths = (dijkstra start #'dir-close)
         for end in (list :up :down :left :right :a)
         do (progn
              (format t "~A ~A ~A~%" start end (gethash end paths))
              (setf (gethash (cons start end) *dirpad-paths*) (gethash end paths))))))

(defun init-numpad-paths ()
  (setf *numpad-paths* (make-hash-table :test #'equal))
  (loop
    for start in (list :0 :1 :2 :3 :4 :5 :6 :7 :8 :9 :a)
    do (loop
         with paths = (dijkstra start #'num-close)
         for end in (list :0 :1 :2 :3 :4 :5 :6 :7 :8 :9 :a)
         do (setf (gethash (cons start end) *numpad-paths*) (gethash end paths)))))

(init-dirpad-paths)
(init-numpad-paths)

(gethash (cons :left :up)  *dirpad-paths*)

(defun move-to-dir-key (move)
  (cond ((equal move (cons -1 0)) :up)
        ((equal move (cons 1 0)) :down)
        ((equal move (cons 0 1)) :right)
        ((equal move (cons 0 -1)) :left)))

(defun dir-move-to-dir-key (dirk1 dirk2)
  (let ((cell1 (dirkey-to-cell dirk1))
        (cell2 (dirkey-to-cell dirk2)))
    (move-to-dir-key (cons (- (car cell2) (car cell1)) (- (cdr cell2) (cdr cell1))))))

(defun num-move-to-dir-key (numk1 numk2)
  (let ((cell1 (numkey-to-cell numk1))
        (cell2 (numkey-to-cell numk2)))
    (move-to-dir-key (cons (- (car cell2) (car cell1)) (- (cdr cell2) (cdr cell1))))))


;;(gethash (cons :up :right) *dirpad-paths*)
;;(gethash (cons :a :8) *numpad-paths*)
(defvar *dir-moves-cache*)
(defvar *num-moves-cache*)

(defun init-move-caches ()
  (setf *dir-moves-cache* (make-hash-table :test #'equal))
  (setf *num-moves-cache* (make-hash-table :test #'equal)))

(init-move-caches)

(defun get-dirpad-moves (start end)
  (or (gethash (cons start end) *dir-moves-cache* )
      (setf (gethash (cons start end) *dir-moves-cache* )
            (let ((path (gethash (cons start end) *dirpad-paths*)))
              (sort (mapcar (lambda (pair) (dir-move-to-dir-key (car pair) (cdr pair)))
                            (loop for ck = start then nk
                                  for nk in path
                                  collect (cons ck nk)))
                    (lambda (x y) (string< (string x) (string y))))))))

(defun get-numpad-moves (start end)
  (or (gethash (cons start end) *num-moves-cache*)
      (setf (gethash (cons start end) *num-moves-cache*)
            (let ((path (gethash (cons start end) *numpad-paths*)))
              (sort (mapcar (lambda (pair) (num-move-to-dir-key (car pair) (cdr pair)))
                            (loop for ck = start then nk
                                  for nk in path
                                  collect (cons ck nk)))
                    (lambda (x y) (string< (string x) (string y))))))))

(defun initial-costs ()
  (loop with costs = (make-hash-table :test #'equal)
        for start in (list :up :left :right :down :a) do
          (loop for end in (list :up :left :right :down :a) do
            (progn
              (setf (gethash (cons start end) costs) 0)))
        finally (return costs)))

(defun print-costs (costs)
  (loop for key being the hash-keys in costs using (hash-value cost)
        do (format t "~A : ~A~%" key cost)))

;;; USELESS:
(defun next-dir-costs (costs) ;;
  (loop with next-costs = (make-hash-table :test #'equal)
        for start in (list :up :left :right :down :a) do
          (loop for end in (list :up :left :right :down :a) do
            (setf (gethash (cons start end) next-costs)
                  (loop for prev = :a then m
                        for m in (get-dirpad-moves start end)
                        summing (1+ (gethash (cons prev m) costs)))))
        finally (return next-costs)))

(defun real-cost (start end n)
  (if (<= n 0)
      1
      (1+ (loop
            for prev = :a then move
            for move in (get-dirpad-moves start end)
            summing (real-cost prev move (1- n))))))

(let ((steps 1))
  (+
   (real-cost :a :left 1)
   1
   (real-cost :left :up 1)
   1
   (real-cost :up :right steps)
   (real-cost :right :up steps)
   (real-cost :up :up steps)
   1
   (real-cost :up :down steps)
   (real-cost :down :down steps)
   (real-cost :down :down steps)
   1))


(defun intermediate (moves start)
  "build the dir moves that must be pressed to produce input moves, if there is an intermediate starting in start"
  (loop
    for prev = start then move
    for move in moves
    appending (append (get-dirpad-moves prev move)
                      (list :a))))

(loop for moves = (append (get-numpad-moves :a :0) (list :a)
                          (get-numpad-moves :0 :2) (list :a)
                          (get-numpad-moves :2 :9) (list :a)
                          (get-numpad-moves :9 :a) (list :a)) then (intermediate moves :a)
      for i from 0 below 2
      finally (return (length moves)))

(defun intermediate-n (moves start n)
  "build the dir moves that must be pressed to produce input moves, if there is an intermediate starting in start"
  (if (zerop n)
      moves
      (loop
        for prev = start then move
        for move in moves
        appending (intermediate-n (append (get-dirpad-moves prev move) (list :a)) :a (1- n)))))

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

(* 28 )

(let ((memo (make-hash-table :test #'equal)))
  (intermediate-n-cost (append (get-numpad-moves :a :0) (list :a)
                               (get-numpad-moves :0 :2) (list :a)
                               (get-numpad-moves :2 :9) (list :a)
                               (get-numpad-moves :9 :a) (list :a)) :a 2 memo))
