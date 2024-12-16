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

(defstruct state
  map
  step
  position
  moves)

(defun dir (c)
  (ecase c
    (#\< (cons 0 -1))
    (#\^ (cons -1 0))
    (#\> (cons 0 1))
    (#\v (cons 1 0))))

(defun target (dir pos)
  (make-pos (+ (car pos) (car dir)) (+ (cdr pos) (cdr dir))))

(defun read-map (stream)
  "return the map and the initial position"
  (loop with lines = (loop for line = (read-line stream nil) while (not (string= line "")) collect line)
        with initial-position = nil
        with m = (make-array (list  (length lines) (length (first lines))))
        for line in lines
        for row from 0 below (rows m)
        do (loop for col from 0 below (cols m)
                 do (let ((char (aref line col)))
                      (when (char= char #\@)
                        (setf initial-position (make-pos row col)))
                      (setf (aref m row col)
                            (ecase char
                              (#\# :wall) (#\. :empty) (#\@ :empty) (#\O :box)))))
        finally (return (values m initial-position))))

(defun read-moves (stream)
  (apply #'concatenate 'string
         (loop for line = (read-line stream nil)
               while line collect line)))

(defun read-input (file)
  (with-open-file (s file)
    (multiple-value-bind (m pos) (read-map s)
      (make-state
       :map m
       :position pos
       :step 0
       :moves (read-moves s)))))

(defun has-empty-after-boxes (map pos dir)
  "expects pos to contain box"
  (loop
    for cpos = (target dir pos) then (target dir cpos)
    while (eq (mval map cpos) :box)
    finally (return (eq (mval map cpos) :empty))))

(defun push-boxes (map pos dir)
  "expects pos to contain box and to be an empty space after boxes"
  ;;  (format t "pushing boxes from ~A~%" pos)
  (setf (aref map (car pos) (cdr pos)) :empty)
  (loop
    for cpos = (target dir pos) then (target dir cpos)
    while (eq (mval map cpos) :box)
    finally (setf (aref map (car cpos) (cdr cpos)) :box)))

(defun make-step (state)
  (let* ((move (aref (state-moves state) (state-step state)))
         (dir (dir move))
         (target (target dir (state-position state)))
         (target-val (mval (state-map state) target)))
    (incf (state-step state))
    (case target-val
      (:empty (setf (state-position state) target))
      (:box (when (has-empty-after-boxes (state-map state) target dir)
              ;;              (print "has empty!")
              ;;             (terpri)
              (push-boxes (state-map state) target dir)
              (setf (state-position state) target)
              )))))

(defun print-map (state)
  (loop for row from 0 below (rows (state-map state))
        do (progn
             (loop for col from 0 below (cols (state-map state))
                   do (if (equal (cons row col) (state-position state))
                          (princ "@")
                          (princ (case (mval (state-map state) (cons row col))
                                   (:empty ".")
                                   (:box "O")
                                   (:wall "#")))))
             (terpri))))

(defun map-value (state)
  (loop for row from 0 below (rows (state-map state))
        summing (loop for col from 0 below (cols (state-map state))
                      summing (if (eq :box (mval (state-map state) (make-pos row col)))
                                  (+ (* 100 row) col)
                                  0))))

;; part1

(let ((state (read-input "input")))
  ;;  (print "initial state")
  ;;  (terpri)
  ;;  (print-map state)
  (dotimes (i (length (state-moves state)))
    ;;  (print i)
    ;;  (print (aref (state-moves state) i))
    ;;  (terpri)
    (make-step state)
    ;;(print-map state)
    )
  (print-map state)
  (print (map-value state)))

;; part 2
(defun double-map (state)
  (make-state
   :map (loop
          with new-map = (make-array (list (rows (state-map state)) (* 2 (cols (state-map state)))))
          for row from 0 below (rows (state-map state))
          do (loop for col from 0 below (cols (state-map state))
                   do (let ((v (mval (state-map state) (cons row col))))
                        (case v
                          (:wall (setf (aref new-map row (* 2 col)) :wall
                                       (aref new-map row (1+ (* 2 col))) :wall))
                          (:empty (setf (aref new-map row (* 2 col)) :empty
                                        (aref new-map row (1+ (* 2 col))) :empty))
                          (:box (setf (aref new-map row (* 2 col)) :left-box
                                      (aref new-map row (1+ (* 2 col))) :right-box)))))
          finally (return new-map))
   :position (cons (car (state-position state)) (* 2 (cdr (state-position state))))
   :step (state-step state)
   :moves (state-moves state)))

(defun print-map2 (state)
  (loop for row from 0 below (rows (state-map state))
        do (progn
             (loop for col from 0 below (cols (state-map state))
                   do (if (equal (cons row col) (state-position state))
                          (princ "@")
                          (princ (case (mval (state-map state) (cons row col))
                                   (:empty ".")
                                   (:left-box "[")
                                   (:right-box "]")
                                   (:wall "#")))))
             (terpri))))

(defun horizontal-dir (dir)
  (not (zerop (cdr dir))))

(defun vertical-dir (dir)
  (not (zerop (car dir))))

(defun can-move-to (state pos dir)
  (let* ((m (state-map state))
         (target (target dir pos))
         (target-val (mval m target)))
    (cond ((eq target-val :wall) nil)
          ((eq target-val :empty) t)
          (t (if (horizontal-dir dir)
                 (can-move-to state target dir)
                 (if (eq target-val :left-box)
                     (and (can-move-to state target dir)
                          (can-move-to state (cons (car target) (1+ (cdr target))) dir))
                     (and (can-move-to state target dir)
                          (can-move-to state (cons (car target) (1- (cdr target))) dir))))))))

(defun move-everything (state pos dir)
  "assumes boxes can be pushed"
  (let* ((m (state-map state))
         (target (target dir pos))
         (target-val (mval m target))
         (targets (cond ((and (vertical-dir dir) (eq target-val :left-box))
                         (list target (cons (car target) (1+ (cdr target)))))
                        ((and (vertical-dir dir) (eq target-val :right-box))
                         (list target (cons (car target) (1- (cdr target)))))
                        ((and (horizontal-dir dir) (not (eq target-val :empty)))
                         (list target))
                        (t '()))))
    (loop for target in targets
          do (move-everything state target dir))
    (let ((val (mval (state-map state) pos)))
      (setf (aref (state-map state) (car pos) (cdr pos)) :empty)
      (setf (aref (state-map state) (car target) (cdr target)) val))))

(defun make-step2 (state)
  (let* ((move (aref (state-moves state) (state-step state)))
         (dir (dir move))
         (target (target dir (state-position state))))
    (incf (state-step state))
    (when (can-move-to state (state-position state) dir)
      (move-everything state (state-position state) dir)
      (setf (state-position state) target))))

(defun map-value2 (state)
  (loop for row from 0 below (rows (state-map state))
        summing (loop for col from 0 below (cols (state-map state))
                      summing (if (eq :left-box (mval (state-map state) (make-pos row col)))
                                  (+ (* 100 row) col)
                                  0))))

(let* ((state (read-input "input"))
       (doubled (double-map state)))
  (print "initial state")
  (terpri)
  (print-map state)
  (dotimes (i (length (state-moves doubled)))
    (print i)
    (print (aref (state-moves state) i))
    (terpri)
    (make-step2 doubled)
    )
  (print-map2 doubled)
  (print (map-value2 doubled)))
