;; visited: hashmap (row . col) -> direction -> T
(setf *print-array* t)
(defun make-visited () (make-hash-table :test #'equal))
(defun visit (v pos dir)
  (when (not (gethash pos v)) (setf (gethash pos v) (make-hash-table :test #'equal)))
  (setf (gethash dir (gethash pos v)) t))

(defun visit-pos-p (v pos) (gethash pos v))
(defun visit-pos-dir-p (v pos dir) (and (gethash pos v) (gethash dir (gethash pos v))))

;; maps: hashmap (row . col) -> T when obstacle
(defstruct mapp obstacles rows cols position direction visited)
(defun make-map () (make-mapp :obstacles (make-hash-table :test #'equal)
                              :visited (make-visited)))
(defun set-obstacle (map pos) (setf (gethash pos (mapp-obstacles map))  t))
(defun obstacle-p (map pos) (gethash pos (mapp-obstacles map)))
(defun in-map-p (map)
  (and (>= (car (mapp-position map)) 0)
       (>= (cdr (mapp-position map)) 0)
       (< (car (mapp-position map)) (mapp-rows map))
       (< (cdr (mapp-position map)) (mapp-cols map))))

(defun adj-pos (pos dir)
  (case dir
    (:UP (cons (1- (car pos)) (cdr pos)))
    (:DOWN (cons (1+ (car pos)) (cdr pos)))
    (:LEFT (cons (car pos) (1- (cdr pos))))
    (:RIGHT (cons (car pos) (1+ (cdr pos))))))

(defun turn-right (dir) (case dir (:UP :RIGHT) (:RIGHT :DOWN) (:DOWN :LEFT) (:LEFT :UP)))

(defun next-step (map)
  "advances state to next step"
  (let ((adj (adj-pos (mapp-position map) (mapp-direction map))))
    (cond ((obstacle-p map adj)
           (setf (mapp-direction map) (turn-right (mapp-direction map)))
           (visit (mapp-visited map)
                  (mapp-position map)
                  (turn-right (mapp-direction map))))
          (t (setf (mapp-position map) adj)
             (visit (mapp-visited map)
                    adj
                    (mapp-direction map)))))
  map)

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
      while (in-map-p map)
      do (next-step map)
      finally (return (1- (hash-table-count (mapp-visited map)))))
