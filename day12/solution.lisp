(defun read-input (file)
  (let* ((lines (with-open-file (s file)
                  (loop for line = (read-line s nil) while line collect line)))
         (rows (length lines))
         (cols (length (first lines)))
         (m (make-array (list rows cols))))
    (loop for line in lines
          for  row from 0 below rows
          do (loop for col from 0 below cols do (setf (aref m row col) (char-code (aref line col))))) m))

(defun rows (m) (array-dimension m 0))
(defun cols (m) (array-dimension m 1))
(defun row (pos) (car pos))
(defun col (pos) (cdr pos))

(defun up (pos) (make-pos (1- (row pos)) (col pos)))
(defun down (pos) (make-pos (1+ (row pos)) (col pos)))
(defun left (pos) (make-pos (row pos) (1- (col pos))))
(defun right (pos) (make-pos (row pos) (1+ (col pos))))

(defun within-bounds (m pos)
  (and (>= (row pos) 0) (>= (col pos) 0)
       (< (row pos) (rows m)) (< (col pos) (cols m))))

(defun adjs (m pos)
  (loop for adj in (list (up pos) (down pos) (left pos) (right pos))
        when (within-bounds m adj) collect adj))

(defun frontiers-count (m pos)
  (let ((adjs (adjs m pos)))
    (+ (- 4 (length adjs))
       (loop for adj in adjs
             counting
             (not (= (aref m (row pos) (col pos))
                     (aref m (row adj) (col adj))))))))

;; equivalences
(defun make-links () (make-hash-table :test #'equal))

(defun set-link (l1 l2 links)
  (setf (gethash (cons (min l1 l2) (max l1 l2)) links) t))

(defun is-linked (l1 l2 links)
  (or (= l1 l2)
      (gethash (cons (min l1 l2) (max l1 l2)) links)))

(defun max-linked (link links max)
  (loop for candidate from max downto link
        while (not (is-linked candidate link links))
        finally (return candidate)))

(defun follow-links-up (label links max)
  (loop for l = label then (max-linked l links max)
        while (not (= (max-linked l links max) l))
        finally (return l)))

(defun links-to-assoc (links)
  (loop for couple being the hash-keys of links collecting couple))

(defun groups (links max)
  "the highest label transitively linked to each label"
  (loop with groups = (make-hash-table)
        for l from 0 upto max
        do (setf (gethash l groups) (follow-links-up l links max))
        finally (return groups)))

(defun groups2 (links max)
  "the highest label transitively linked to each label"
  (loop with groups = (loop for l from 0 upto max collecting (list l))
        for link in (links-to-assoc links) do
          (let ((g1 (find-if (lambda (g) (find (car link) g)) groups))
                (g2 (find-if (lambda (g) (find (cdr link) g)) groups)))
            (when (not (eq g1 g2))
              (setf groups
                    (cons
                     (append g1 g2)
                     (remove g2 (remove g1 groups))
                     ))))
        finally (return (loop
                          with m = (make-hash-table)
                          for l from 0 upto max
                          do
                             (let ((g (find-if (lambda (g) (find l g)) groups)))
                               (setf (gethash l m) (apply #'max g)))
                          finally (return m)))))

;; part 1
(defun regions (input)
  (loop
    with max-label = 0
    with labels = (make-array (list (rows input) (cols input)))
    with links = (make-links)
    for row from 0 below (rows input)
    do (loop for col from 0 below (cols input)
             do (cond
                  ((and (zerop row) (zerop col))
                   (setf (aref labels row col) max-label))
                  ((zerop row)
                   (cond ((= (aref input row col)
                             (aref input row (1- col)))
                          (setf (aref labels row col)
                                (aref labels row (1- col))))
                         (t (incf max-label) (setf (aref labels row col) max-label))))
                  ((zerop col)
                   (cond ((= (aref input row col)
                             (aref input (1- row) col))
                          (setf (aref labels row col)
                                (aref labels (1- row) col)))
                         (t (incf max-label)
                            (setf (aref labels row col) max-label))))
                  (t (cond ((= (aref input row col)
                               (aref input (1- row) col)
                               (aref input row (1- col)))
                            ;; might not be necessary but set it anyway:
                            (set-link (aref labels (1- row) col) (aref labels row (1- col)) links)
                            (setf (aref labels row col)
                                  (aref labels (1- row) col)))
                           ((and (= (aref input row col) (aref input (1- row) col))
                                 (not (=  (aref input row col) (aref input row (1- col)))))
                            (setf (aref labels row col)
                                  (aref labels (1- row) col)))
                           ((and (= (aref input row col) (aref input row (1- col)))
                                 (not (= (aref input row col) (aref input (1- row) col))))
                            (setf (aref labels row col)
                                  (aref labels row (1- col))))
                           (t (incf max-label)
                              (setf (aref labels row col) max-label))))))
    finally (return (let ((groups2 (groups links max-label))
                          (groups (groups2 links max-label))
                          (regions (make-array (list (rows input) (cols input)))))
                      (loop for k being the hash-keys of groups2 do
                        (format t "~A ~A ~%" k (gethash k groups2)))
                      (loop for row from 0 below (rows regions)
                            do (loop for col from 0 below (cols regions)
                                     do (setf (aref regions row col) (gethash (aref  labels row col) groups))))
                      regions))))

(regions (read-input "test4"))

;; part 1
(let* ((input (read-input "input"))
       (area (make-hash-table))
       (perimeter (make-hash-table))
       (regions (regions input)))
  (print regions)
  (loop for row from 0 below (rows input)
        do (loop for col from 0 below (cols input)
                 do (let ((label (aref regions row col))
                          (frontiers (frontiers-count input (cons row col))))
                      (when (not (gethash label perimeter))
                        (setf (gethash label perimeter) 0))
                      (when (not (gethash label area))
                        (setf (gethash label area) 0))
                      (incf (gethash label perimeter) frontiers)
                      (incf (gethash label area)))))
  (loop for l being the hash-keys in area
        summing
        (progn
          (format t "~A ~A ~A~%" l (gethash l area) (gethash l perimeter))
          (* (gethash l area) (gethash l perimeter)))))

(regions (read-input "test4"))
