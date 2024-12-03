;; Part 1: grep -Po '' input | tr "," "*" | tr -d -c "[:digit:]*\\n" | paste -sd+ | bc

(ql:quickload "cl-ppcre")

(defun m (r s)
  (cl-ppcre:all-matches-as-strings r s))

(defun eval-mult (string)
  (apply #'* (mapcar #'parse-integer (matches "\\d+" string))))

;; part 1
(with-open-file (stream "input")
  (let* ((input (make-string (file-length stream))))
    (read-sequence input stream)
    (reduce #'+
            (mapcar #'eval-mult
                    (m "mul\\(\\d{1,3},\\d{1,3}\\)" input)))))

;; part 2
(with-open-file (stream "input")
  (let* ((input (make-string (file-length stream))))
    (read-sequence input stream)
    (loop
      with enabled = t
      for command in (m "(don't\\(\\))|(do\\(\\))|(mul\\(\\d{1,3},\\d{1,3}\\))" input)
      summing (cond ((string= "do()" command) (setf enabled t) 0)
                    ((string= "don't()" command) (setf enabled nil) 0)
                    (enabled (eval-mult command))
                    (t 0)))))
