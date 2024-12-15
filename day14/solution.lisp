(ql:quickload "cl-ppcre")

(defvar *rows* 103)
(defvar *cols* 101)
(setf *rows* 103)
(setf *cols* 101)
(defun read-input (file)
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while line
          collect (cl-ppcre:register-groups-bind (x y vx vy) ("p=(.+),(.+) v=(.+),(.+)" line)
                    (cons (cons (read-from-string x) (read-from-string  y)) (cons (read-from-string  vx) (read-from-string  vy)))))))

(defun scalar-iterate (offset step times size)
  (if (> step 0)
      (mod (+ offset (* step times)) size)
      (- size (1+ (mod (+ (- size (1+ offset)) (* -1 step times)) size)))))

(defun iterate (pos speed times)
  (cons (scalar-iterate (car pos) (car speed) times *cols*)
        (scalar-iterate (cdr pos) (cdr speed) times *rows*)))

(defun quad (pos)
  "assumes odd cols rows"
  (let ((x (car pos))
        (y (cdr pos))
        (central-column (floor (/ *cols* 2)))
        (central-row (floor (/ *rows* 2))))
    (unless (or (= x central-column) (= y central-row))
      (if (< x central-column)
          (if  (< y central-row) 0 2)
          (if  (< y central-row) 1 3)))))

;; part 1
(defun init-quad-counts ()
  (let ((m (make-hash-table)))
    (setf (gethash 0 m) 0)
    (setf (gethash 1 m) 0)
    (setf (gethash 2 m) 0)
    (setf (gethash 3 m) 0)
    m))

(loop
  with counts = (init-quad-counts)
  for entry in (read-input "input")
  do (let* ((final-position (iterate (car entry) (cdr entry) 100))
            (q (quad final-position)))
       (when q
         (incf (gethash q counts))))
  finally (return (* (gethash 0 counts)(gethash 1 counts)(gethash 2 counts)(gethash 3 counts))))

;; part 2
(defun print-map (positions filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (princ "P1" stream)
    (terpri stream)
    (format stream "~A ~A~%" *cols* *rows*)
    (loop for row from 0 below *rows*
          do (loop for col from 0 below *cols*
                   do (if (find (cons col row) positions :test #'equal)
                          (princ "1 " stream)
                          (princ "0 " stream))
                   finally (terpri stream)))))

(loop with entries = (read-input "input")
      for i from 31 upto 20000 by 103
      do (progn
           (print i)
           (print-map (mapcar (lambda (entry) (iterate (car entry) (cdr entry) i)) entries) (format nil "images/~6,'0d.pbm" i))))

;; and then have a look at the folder images! :D
