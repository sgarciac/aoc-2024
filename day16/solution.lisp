(defstruct input start end map)
(defstruct cpath path cost direction)
(defstruct state visited unvisited)

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

(defun rows (m) (array-dimension m 0))
(defun cols (m) (array-dimension m 1))
(defun row (pos) (car pos))
(defun col (pos) (cdr pos))
(defun val (pos m) (aref m (car pos) (cdr pos)))
(defun next-step (pos dir)
  (cons (+ (car pos) (car dir)) (+ (cdr pos) (cdr dir))))

(defun clockwise (dir)
  (cons (cdr dir) (* -1 (car dir)))  )

(defun anticlockwise (dir)
  (cons (* -1 (cdr dir)) (car dir)))

(defun turnaround (dir)
  (cons  (* -1 (car dir)) (* -1 (cdr dir))))

(defun init-state (m start)
  "visited: position -> path / cost / direction"
  (let ((visited (make-hash-table :test #'equal))
        (unvisited (make-hash-table :test #'equal)))
    (loop for row from 0 below (rows m) do
      (loop for col from 0 below (cols m)
            do (let ((pos (cons row col)))
                 (when (eq (val pos m) :empty)
                   (setf (gethash pos unvisited)
                         (if (equal start pos)
                             (make-cpath :path (list pos) :cost 0 :direction (cons 0 1))
                             (make-cpath :path nil :cost nil :direction nil)))))))
    (make-state :visited visited :unvisited unvisited)))

(defun closest-unvisited (unvisited)
  (loop
    with closest = nil
    for candidate being the hash-values in unvisited
    do (when (and (cpath-cost candidate)
                  (or (not closest)
                      (< (cpath-cost candidate) (cpath-cost closest))))
         (setf closest candidate))
    finally (return closest)))


(let* ((input (read-input "input"))
       (state (init-state (input-map input) (input-start input))))
  (loop
    for current = (closest-unvisited (state-unvisited state))
    while current
    do (let ((pos (car (cpath-path current)))
             (current-dir (cpath-direction current)))
         (format t "~A ~A~%" (cpath-cost current) pos)
         (loop for dir in (list
                           current-dir
                           (clockwise current-dir)
                           (anticlockwise current-dir)
                           (turnaround current-dir))
               for cost in (list 1 1001 1001 1001)
               do (let* ((next-pos (next-step pos dir))
                         (next-cpath (gethash next-pos (state-unvisited state))))
                    (when next-cpath
                      (when (or (not (cpath-cost next-cpath))
                                (< (+ (cpath-cost current) cost) (cpath-cost next-cpath)))
                        (setf (gethash next-pos (state-unvisited state))
                              (make-cpath
                               :path (cons next-pos (cpath-path current))
                               :cost (+ (cpath-cost current) cost)
                               :direction dir))))))
         (setf (gethash pos (state-visited state)) current)
         (remhash pos (state-unvisited state))))
  (gethash (input-end input) (state-visited state)))

(input-end (read-input "input"))
