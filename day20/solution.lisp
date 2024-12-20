(defstruct input start end map)
(defstruct state visited unvisited)

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
        when (eq :empty (aref m (car adj) (cdr adj)))
          collecting adj))


(defun read-input (file)
  (let* ((lines (with-open-file (s file)
                  (loop for line = (read-line s nil) while line collect line)))
         (rows (length lines))
         (cols (length (first lines)))
         (input (make-input :map (make-array (list rows cols)) :start nil :end nil)))
    (loop
      for line in lines
      with startpos = nil
      with endpos = nil
      for row from 0 below rows
      do (loop for col from 0 below cols
               do (cond ((char= (aref line col) #\#)
                         (setf (aref (input-map input) row col) :wall))
                        ((char= (aref line col) #\.)
                         (setf (aref (input-map input) row col) :empty))
                        ((char= (aref line col) #\E)
                         (setf (input-end input) (cons row col))
                         (setf (aref (input-map input) row col) :empty))
                        ((char= (aref line col) #\S)
                         (setf (input-start input) (cons row col))
                         (setf (aref (input-map input) row col) :empty))
                        ))) input))


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


;; part 1, really bad solution:
(let* ((input (read-input "input")))
  (let ((original-solution
          (let ((visited (dijkstra (input-map input) (input-start input))))
            (gethash (input-end input) visited))))
    (print original-solution)
    (loop for row from 0 below (rows (input-map input))
          summing
          (loop for col from 0 below (cols (input-map input))
                summing
                (progn
                  (let ((original-value (aref (input-map input) row col)))
                    (setf (aref (input-map input) row col) :empty)
                    (let ((visited (dijkstra (input-map input) (input-start input))))
                      (setf (aref (input-map input) row col) original-value)
                      (if (>= (- original-solution (gethash (input-end input) visited)) 100) 1 0))))))))


;; part 2 dijkstra all the things to be dijkstraed
(defun distance (pos1 pos2)
  (+ (abs (- (car pos1) (car pos2))) (abs (- (cdr pos1) (cdr pos2)))))

(let* ((input (read-input "input"))
       (original-solution (dijkstra (input-map input) (input-start input)))
       (to-end (make-hash-table :test #'equal)))
  (loop for row from 0 below (rows (input-map input))
        do
           (loop for col from 0 below (cols (input-map input))
                 do (when (eq :empty (aref (input-map input) row col))
                      (setf (gethash (cons row col) to-end)
                            (gethash (input-end input) (dijkstra (input-map input) (cons row col)))))))
  (loop for row from 0 below (rows (input-map input))
        summing
        (loop for col from 0 below (cols (input-map input))
              summing
              (if (eq :empty (aref (input-map input) row col))
                  (loop for row2 from 0 below (rows (input-map input))
                        summing
                        (loop for col2 from 0 below (cols (input-map input))
                              summing
                              (if (and (eq :empty (aref (input-map input) row2 col2)) (<= (distance (cons row col) (cons row2 col2)) 20 ))
                                  (if (>= (- (gethash (input-end input) original-solution)
                                             (+
                                              (distance (cons row col) (cons row2 col2))
                                              (gethash (cons row col) original-solution)
                                              (gethash (cons row2 col2) to-end)                                              )) 100) 1 0) 0))) 0))))
