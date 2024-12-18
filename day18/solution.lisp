(ql:quickload "cl-ppcre")
(defun read-input (file)
  (with-open-file (s file)
    (loop
      for line = (read-line s nil)
      with entries = ()
      while line
      collecting (cl-ppcre:do-register-groups (col row) ("(\\d+),(\\d+)" line)
                   (push (cons (parse-integer row) (parse-integer col)) entries))
      finally (return (make-array (list (length entries)) :initial-contents
                                  (reverse entries))))))

(read-input "test")

(defun load-memory (entries rows cols n)
  (loop with m = (make-array (list rows cols) :initial-element nil)
        for i from 0 below n
        do (setf (aref m (car (aref entries i)) (cdr (aref entries i))) t)
        finally (return m)))

(defun rows (m) (array-dimension m 0))
(defun cols (m) (array-dimension m 1))
(defun row (pos) (car pos))
(defun col (pos) (cdr pos))
(defun make-pos (row col) (cons row col))
(defun up (pos) (make-pos (1- (row pos)) (col pos)))
(defun down (pos) (make-pos (1+ (row pos)) (col pos)))
(defun left (pos) (make-pos (row pos) (1- (col pos))))
(defun right (pos) (make-pos (row pos) (1+ (col pos))))
(defun mval (m pos) (aref m (car pos) (cdr pos)))
(defun within-bounds (m pos)
  (and (>= (row pos) 0) (>= (col pos) 0)
       (< (row pos) (rows m)) (< (col pos) (cols m))))

(defun adjs (m pos)
  (loop for adj in (list (up pos) (down pos) (left pos) (right pos))
        when (within-bounds m adj) collect adj))

(defun empty-adjs (m pos)
  (loop for adj in (adjs m pos)
        when (not (aref m (car adj) (cdr adj)))
          collecting adj))

(defstruct state
  unvisited
  visited)

(defun init-state (pos)
  (let ((state (make-state
                :unvisited (make-hash-table :test #'equal)
                :visited (make-hash-table :test #'equal)
                )))
    (setf (gethash pos (state-unvisited state)) 0)
    state))

(defun closest-unvisited (state)
  (loop
    with closest = nil
    for position being the hash-keys in (state-unvisited state) using (hash-value distance)
    do (when (or (not closest)
                 (< distance
                    (gethash closest (state-unvisited state))))
         (setf closest position))
    finally (return closest)))

(defun empty-adjs-non-visited (pos memory state)
  (loop for adj in (empty-adjs memory pos)
        when (not (gethash adj (state-visited state))) collecting adj))

(defun dijkstra (memory start)
  (loop
    with state = (init-state start)
    for closest = (closest-unvisited state)
    while closest
    do (let* ((adjs (empty-adjs-non-visited closest memory state)))
         (loop for adj in adjs
               do (when (or (not (gethash adj (state-unvisited state)))
                            (< (1+ (gethash closest (state-unvisited state)))
                               (gethash adj (state-unvisited state))))
                    (setf (gethash adj (state-unvisited state)) (1+ (gethash closest (state-unvisited state))))))
         (setf (gethash closest (state-visited state)) (gethash closest (state-unvisited state)))
         (remhash closest (state-unvisited state)))
    finally (return (state-visited state))))

;; part1
(let* ((memory (load-memory (read-input "input") 71 71 1024)))
  (gethash (cons 70 70) (dijkstra memory (cons 0 0))))

;; part 2

(let* ((entries (read-input "input")))
  (loop for i from 0 below (length entries)
        for memory = (load-memory entries 71 71 i)
        for cost = (gethash (cons 70 70) (dijkstra memory (cons 0 0)))
        while cost
        finally (return (aref entries (1- i)))))
