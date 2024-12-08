(defun read-input (file)
  (let* ((lines (with-open-file (s file)
                  (loop for line = (read-line s nil) while line collect line)))
         (rows (length lines))
         (cols (length (first lines)))
         (m (make-array (list rows cols))))
    (loop for line in lines
          for  row from 0 below rows
          do (loop for col from 0 below cols do (setf (aref m row col) (aref line col)))) m))

(defun rows (m) (array-dimension m 0))
(defun cols (m) (array-dimension m 1))
(defun val (m pos) (aref m (car pos) (cdr pos)))
(defun within-bounds (m pos) (and (>= (car pos) 0) (>= (cdr pos) 0)
                                  (< (car pos) (rows m)) (< (cdr pos) (cols m))))
(defun is-antenna (m row col) (char/= (val m (cons row col)) #\.))

(defun antenna-positions (m)
  "returns all the positions with an antenna"
  (loop for row from 0 below (rows m)
        appending (loop for col from 0 below (cols m)
                        when (is-antenna m row col) collecting (cons row col))))

(defun distinct-frequencies (m antennas)
  "returns all the distinct frequencies from a map and a list of antenna positions"
  (remove-duplicates (mapcar (lambda (pos) (val m pos)) antennas)))

(defun group-by-frequency (m antennas)
  "returns a hashmap frequency -> list of antenna positions from a map ant the antenna positions"
  (loop
    with frequencies = (make-hash-table)
    for antenna in antennas
    do (let ((frequency (val m antenna)))
         (if (not (gethash frequency frequencies))
             (setf (gethash frequency frequencies) (list antenna))
             (push antenna (gethash frequency frequencies))))
    finally (return frequencies)))

(defun make-couples (x l)
  "makes a list of couples whose first element is x and the second is each element of l in turn"
  (loop for y in l collect (cons x y)))

(defun all-pairs (l)
  "makes a list of all the possible pairs of elements of list l"
  (loop for (x . y) on l nconc (make-couples x y)))

(defun antinodes (pos1 pos2)
  (let ((rdiff (- (car pos2) (car pos1)))
        (cdiff (- (cdr pos2) (cdr pos1))))
    (list
     (cons (- (car pos1) rdiff) (- (cdr pos1) cdiff))
     (cons (+ (car pos2) rdiff) (+ (cdr pos2) cdiff)))))

(defun all-antinodes (m frequencies groups)
  (loop for f in frequencies
        appending
        (loop for pair in (all-pairs (gethash f groups))
              appending (let ((ans (antinodes (car pair) (cdr pair))))
                          (loop for an in ans when (within-bounds m an) collecting an)))))
;; part 1
(let* ((m (read-input "input"))
       (antennas (extract-antennas m))
       (frequencies (distinct-frequencies m antennas))
       (groups (group-by-frequency m antennas)))
  (length (remove-duplicates (all-antinodes m frequencies groups) :test #'equal)))

;; part 2
(defun antinodes2 (m pos1 pos2)
  (let ((rdiff (- (car pos2) (car pos1)))
        (cdiff (- (cdr pos2) (cdr pos1))))
    (append
     (loop for i = 0 then (1+ i)
           for antinode = (cons (+ (car pos2) (* i  rdiff)) (+ (cdr pos2) (* i cdiff)))
           while (within-bounds m antinode)
           collect antinode)
     (loop for i = 0 then (1+ i)
           for antinode = (cons (- (car pos1) (* i rdiff)) (- (cdr pos1) (* i  cdiff)))
           while (within-bounds m antinode)
           collect antinode))))

(defun all-antinodes2 (m frequencies groups)
  (loop for f in frequencies
        appending
        (loop for pair in (all-pairs (gethash f groups))
              appending (let ((ans (antinodes2 m (car pair) (cdr pair))))
                          (loop for an in ans when (within-bounds m an) collecting an)))))

(let* ((m (read-input "input"))
       (antennas (extract-antennas m))
       (frequencies (distinct-frequencies m antennas))
       (groups (group-by-frequency m antennas)))
  (length (remove-duplicates (all-antinodes2 m frequencies groups) :test #'equal)))
