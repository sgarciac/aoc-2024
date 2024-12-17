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

(defun init-state (start dir)
  "visited: position -> path / cost / direction"
  (let ((visited (make-hash-table :test #'equal))
        (unvisited (make-hash-table :test #'equal)))
    (setf (gethash (cons start dir) unvisited)
          (make-cpath :path (list start) :cost 0 :direction dir))
    (make-state :visited visited :unvisited unvisited)))

(defun closest-unvisited (unvisited)
  (loop
    with closest = nil
    for candidate being the hash-values in unvisited
    do (when (or (not closest)
                 (< (cpath-cost candidate) (cpath-cost closest)))
         (setf closest candidate))
    finally (return closest)))

(defun dijkstra (map start dir)
  "returns visited"
  (let* ((state (init-state start dir)))
    (loop
      ;; current = current cpath
      for current = (closest-unvisited (state-unvisited state))
      while current
      do (let* ((pos (car (cpath-path current)))
                (current-dir (cpath-direction current))
                (node (cons pos current-dir)))
           (loop for dir in (list
                             current-dir
                             (clockwise current-dir)
                             (anticlockwise current-dir)
                             (turnaround current-dir))
                 for cost in (list 1 1001 1001 1001)
                 do (let* ((next-pos (next-step pos dir))
                           (next-node (cons next-pos dir))
                           (next-cpath (gethash next-node (state-unvisited state))))
                      (when (eq :empty (val next-pos map))
                        (when
                            (and (not (gethash next-node (state-visited state)))
                                 (or (not next-cpath)
                                     (< (+ (cpath-cost current) cost) (cpath-cost next-cpath))))
                          (setf (gethash next-node (state-unvisited state))
                                (make-cpath
                                 :path (cons next-pos (cpath-path current))
                                 :cost (+ (cpath-cost current) cost)
                                 :direction dir))))))
           (setf (gethash node (state-visited state)) current)
           (remhash node (state-unvisited state))))
    (state-visited state)))

;; part 1
(defun cpaths (pos visited)
  (loop for dir in (list (cons -1 0) (cons 0 1) (cons 1 0) (cons 0 -1))
        for cpath = (gethash (cons pos dir) visited)
        when cpath collecting cpath))

(let* ((input (read-input "input"))
       (visited (dijkstra (input-map input) (input-start input) (cons 0 1))))
  (loop for cpath in (cpaths (input-end input) visited)
        minimizing (cpath-cost cpath)))

;; part 2
(print  (let* ((input (read-input "input"))
               (visited (dijkstra (input-map input) (input-start input) (cons 0 1)))
               (best-final-cost (loop for cpath in (cpaths (input-end input) visited)
                                      minimizing (cpath-cost cpath))))
          (loop for node being the hash-keys of visited using (hash-value cpath)
                for i = 0 then (1+ i)
                with valid = ()
                when (< (cpath-cost cpath) best-final-cost)
                  do (progn (when (zerop (mod i 100))  (print i))
                            (let* ((tvisited (dijkstra (input-map input) (car node) (cdr node)))
                                   (best-intermediate-to-end (loop for cpath in (cpaths (input-end input) tvisited)
                                                                   minimizing (cpath-cost cpath)))
                                   (best-start-to-intermediate
                                     (cpath-cost cpath)))
                              (when (= best-final-cost (+ best-intermediate-to-end best-start-to-intermediate))
                                (push (car node) valid))))
                finally (return (1+ (length (remove-duplicates valid :test #'equal)))))))
