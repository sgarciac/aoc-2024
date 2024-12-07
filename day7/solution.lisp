(ql:quickload 'cl-ppcre)
(ql:quickload 'str)

(defun load-input (file)
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
          collecting (mapcar #'parse-integer (cl-ppcre:split "[ :]+" line)))))

;; part1
(defun solves (target terms)
  (cond
    ((< target 0) nil)
    ((null (cadr terms))
     (= target (car terms)))
    (t (or
        (solves (- target (car terms)) (cdr terms))
        (and (zerop (mod target (car terms))) (solves (/ target (car terms)) (cdr terms)))))))

(loop for entry in (load-input "input")
      when  (solves (car entry) (reverse (cdr entry))) sum (car entry))

;; part2
(defun concat-ints (i1 i2) (parse-integer (format nil "~A~A" i1 i2)))

(defun ends-with (i1 i2) (str:ends-with-p (write-to-string i2) (write-to-string i1)))

(defun remove-suffix (i1 i2)
  (if (= i1 i2) -1
      (parse-integer  (str:substring 0 (* -1 (length (write-to-string i2))) (write-to-string i1)))))

(defun solves2 (target terms)
  (cond
    ((< target 0) nil)
    ((null (cadr terms))
     (= target (car terms)))
    (t
     (or
      (solves2 (- target (car terms)) (cdr terms))
      (and (zerop (mod target (car terms))) (solves2 (/ target (car terms)) (cdr terms)))
      (and (ends-with target (car terms)) (solves2 (remove-suffix target (car terms)) (cdr terms)))))))

(loop
  for entry in (load-input "input")
  when  (solves2 (car entry) (reverse (cdr entry)))
    sum (car entry))
