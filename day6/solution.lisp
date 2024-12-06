;; visited: hashmap (row . col) -> direction -> T
(setf *print-array* t)
(defun make-visited () (make-hash-table :test #'equal))
(defun visit (v pos dir)
  (when (not (gethash pos v)) (setf (gethash pos v) (make-hash-table)))
  (setf (gethash dir (gethash pos v)) t))

(defun visit-pos-p (v pos) (gethash pos v))
(defun visit-pos-dir-p (v pos dir) (and (gethash pos v) (gethash dir (gethash pos v))))

;; maps: hashmap (row . col) -> T when obstacle
(defstruct mapp obstacles rows cols position direction visited)
(defun make-map () (make-mapp :obstacles (make-hash-table :test #'equal)
                              :visited (make-visited)))
(defun set-obstacle (map pos) (setf (gethash pos (mapp-obstacles map))  t))
(defun obstacle-p (map pos) (gethash pos (mapp-obstacles map)))

(defun within-bounds-p (pos rows cols)
  (and (>= (car pos) 0)
       (>= (cdr pos) 0)
       (< (car pos) rows)
       (< (cdr pos) cols)))

(defun in-map-p (map)
  (within-bounds-p (mapp-position map) (mapp-rows map) (mapp-cols map)))

(defun adj-pos (pos dir)
  (case dir
    (:UP (cons (1- (car pos)) (cdr pos)))
    (:DOWN (cons (1+ (car pos)) (cdr pos)))
    (:LEFT (cons (car pos) (1- (cdr pos))))
    (:RIGHT (cons (car pos) (1+ (cdr pos))))))

(defun turn-right (dir) (case dir (:UP :RIGHT) (:RIGHT :DOWN) (:DOWN :LEFT) (:LEFT :UP)))

(defun next-step (map)
  "advances state to next step if possible. returns whether it was :OK, :LOOP or :OUT-OF-BOUNDS"
  (let ((adj (adj-pos (mapp-position map) (mapp-direction map))))
    (when (out-of-bounds-p adj (mapp-rows map) (mapp-cols map)) (return-from next-step :OUT-OF-BOUNDS))
    (cond ((obstacle-p map adj)
           (let ((next-direction (turn-right (mapp-direction map))))
             (cond ((visit-pos-dir-p (mapp-visited map) (mapp-position map) next-direction)
                    (return-from next-step :LOOP))
                   (t
                    (setf (mapp-direction map) next-direction)
                    (visit (mapp-visited map)
                           (mapp-position map)
                           next-direction)))))
          (t
           (cond ((visit-pos-dir-p (mapp-visited map) adj (mapp-direction map))
                  (return-from next-step :LOOP))
                 (t
                  (setf (mapp-position map) adj)
                  (visit (mapp-visited map)
                         adj
                         (mapp-direction map)))))))
  :OK)

(defun load-input (file)
  (with-open-file (s file)
    (loop
      with mapp = (make-map)
      for line = (read-line s nil)
      for row = 0 then (1+ row)
      while line
      do (loop for char across line
               for col = 0 then (1+ col)
               do (case char
                    (#\# (set-obstacle mapp `(,row . ,col)))
                    (#\^
                     (setf (mapp-position mapp) `(,row . ,col))
                     (setf (mapp-direction mapp) :UP)
                     (visit (mapp-visited mapp) `(,row . ,col) :UP)))
               finally (setf (mapp-cols mapp) (1+ col)))
      finally (return (progn (setf (mapp-rows mapp) row) mapp)))
    ))

;; part 1
(loop with map = (load-input "input")
      for result = (next-step map)
      while (eq result :OK)
      finally (return (hash-table-count (mapp-visited map))))

;; part 2
(defun final-result (map)
  (loop
    for result = (next-step map)
    while (eq result :OK)
    finally (return result)))

(let ((map (load-input "input")))
  (loop for row from 0 below (mapp-rows map)
        summing
        (loop for col from 0 below (mapp-cols map)
              counting
              (and (not (obstacle-p map (cons row col)))
                   (not (equal (mapp-position map) (cons row col)))
                   (let ((map-copy (load-input "input")))
                     (setf (gethash (cons row col) (mapp-obstacles map-copy)) t)
                     (eq (final-result map-copy) :LOOP))))))
