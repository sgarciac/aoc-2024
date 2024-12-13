(ql:quickload :cl-ppcre)
(ql:quickload :computable-reals)
(use-package :computable-reals)

(defun vsize (v)
  (sqrt-r (+r (*r (car v) (car v)) (*r (cdr v) (cdr v)))))

(defun dotprod (v1 v2)
  (+r (*r (car v1) (car v2)) (*r (cdr v1) (cdr v2))))

(defun acos-r (x)
  (atan-r (/r (sqrt-r (-r 1 (expt-r x 2))) x)))

(defun angle (v1 v2)
  (acos-r
   (/r (dotprod v1 v2) (*r (vsize v1) (vsize v2)))))

(defun side1 (base angle1 angle2)
  "using law of sins"
  (let ((angle3 (-r +pi-r+ (+r angle1 angle2))))
    (*r base (/r (sin-r angle2) (sin-r angle3)))))

(defun side2 (base angle1 angle2)
  "using law of sins"
  (let ((angle3 (-r +pi-r+ (+r angle1 angle2))))
    (*r base (/r (sin-r angle1) (sin-r angle3)))))

(defun solve (file adding)
  (let ((lines
          (remove
           ""
           (with-open-file (s file)
             (loop for line = (read-line s nil) while line collecting line))
           :test #'string=)))
    (loop for (l1 l2 l3) on lines by #'cdddr
          summing (let* ((deltab1 (cl-ppcre:register-groups-bind (x y) ("Button A: X\\+(\\d+), Y\\+(\\d+)$" l1) (cons  (parse-integer x) (parse-integer y))))
                         (deltab2 (cl-ppcre:register-groups-bind (x y) ("Button B: X\\+(\\d+), Y\\+(\\d+)$" l2) (cons  (parse-integer x) (parse-integer y))))
                         (target (cl-ppcre:register-groups-bind (x y) ("Prize: X=(\\d+), Y=(\\d+)$" l3) (cons (+ adding  (parse-integer x)) (+ adding (parse-integer y)))))
                         (b1
                           (round-r (/r (side1 (vsize target)
                                               (angle deltab1 target)
                                               (angle deltab2 target)) (vsize deltab1))))
                         (b2 (round-r (/r (side2 (vsize target)
                                                 (angle deltab1 target)
                                                 (angle deltab2 target)) (vsize deltab2)))))
                    (if (and (= (car target) (+ (* b1 (car deltab1)) (* b2 (car deltab2))))
                             (= (cdr target) (+ (* b1 (cdr deltab1)) (* b2 (cdr deltab2)))))
                        (+ (* b1 3) b2) 0)))))

(solve "input" 0)
(solve "input" 10000000000000)
