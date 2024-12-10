(defmacro macro-alias (to macro)
  `(setf (macro-function ',to) (macro-function ',macro)))

(macro-alias mvb multiple-value-bind)


(defun read-map (file)
  (let* ((lines (with-open-file (s file)
                  (loop for line = (read-line s nil) while line collect line)))
         (rows (length lines))
         (cols (length (first lines)))
         (m (make-array (list rows cols))))
    (loop for line in lines
          for  row from 0 below rows
          do (loop for col from 0 below cols do (setf (aref m row col) (-  (char-code (aref line col)) 48)))) m))

                                        ; position
(defun make-pos (row col) (cons row col))
(defun row (pos) (car pos))
(defun col (pos) (cdr pos))
(defun up (pos) (make-pos (1- (row pos)) (col pos)))
(defun down (pos) (make-pos (1+ (row pos)) (col pos)))
(defun left (pos) (make-pos (row pos) (1- (col pos))))
(defun right (pos) (make-pos (row pos) (1+ (col pos))))

(defun rows (m) (array-dimension m 0))
(defun cols (m) (array-dimension m 1))
(defun within-bounds (m pos)
  (and (>= (row pos) 0) (>= (col pos) 0)
       (< (row pos) (rows m)) (< (col pos) (cols m))))
(defun height (m pos) (aref m (row pos) (col pos)))
(defun adjs (m pos)
  (loop for adj in (list (up pos) (down pos) (left pos) (right pos))
        when (within-bounds m adj) collect adj))

(defun paths (m pos target visited)
  (let ((height (height m pos)))
    (cond
      ((aref visited (row pos) (col pos))
       (aref visited (row pos) (col pos)))
      ((= height target)
       (setf (aref visited (row pos) (col pos)) (list pos))
       (list pos))
      ((> height target)
       nil)
      (t (loop for adj in (adjs m pos)
               when (= (height m adj) (1+ height))
                 appending (paths m adj target visited) into next-paths
               finally
                  (let ((full-paths (mapcar (lambda (path) (cons pos path)) next-paths)))
                    (setf (aref visited (row pos) (col pos)) full-paths)
                    (return full-paths))
               )))))

;; part1
(let* ((m (read-map "input"))
       (visited (make-array (list (rows m) (cols m)) :initial-element nil)))
  (loop for row from 0 below (rows m)
        do (loop for col from 0 below (cols m)
                 do (paths m (make-pos row col) 9 visited)))
  (print "boo")
  (loop for row from 0 below (rows m)
        summing
        (loop for col from 0 below (cols m)
              summing
              (if (> (height m (make-pos row col)) 0)
                  0
                  (length  (remove-duplicates (mapcar #'last (aref visited row col)) :test #'equal))))))

;; part2
(let* ((m (read-map "input"))
       (visited (make-array (list (rows m) (cols m)) :initial-element nil)))
  (loop for row from 0 below (rows m)
        do (loop for col from 0 below (cols m)
                 do (paths m (make-pos row col) 9 visited)))
  (loop for row from 0 below (rows m)
        summing
        (loop for col from 0 below (cols m)
              summing
              (if (> (height m (make-pos row col)) 0)
                  0
                  (length (aref visited row col))))))
