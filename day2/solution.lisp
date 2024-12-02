(defun read-numbers-from-string (line)
  (with-input-from-string (s line)
    (loop for number = (read s nil)
          while number collecting number)))

(defun differences (numbers)
  (loop for (a b) on numbers by #'cdr while b collecting (- a b)))



(defun safe-p (numbers)
  (let ((diffs (differences numbers)))
    (and
     (every (if (plusp (car diffs) ) #'plusp #'minusp) diffs)
     (every (if (plusp (car diffs)) (lambda (x) (<= x 3)) (lambda (x) (>= x -3))) diffs)
     )))

;; part1
(with-open-file (stream "input")
  (loop for line = (read-line stream nil) while line
        for numbers = (read-numbers-from-string line)
        counting (safe-p numbers)))

;; part 2
(defun without-nth (list n)
  (loop for element in list
        for i = 0 then (1+ i)
        when (not (= i n)) collecting element ))

(defun variations (list)
  (loop for i from 0 below (length list) collecting (without-nth list i)))

(defun less-safe-p (list)
  (or (safe-p list)
      (some #'safe-p (variations list))))

(with-open-file (stream "input")
  (loop for line = (read-line stream nil) while line
        for numbers = (read-numbers-from-string line)
        counting (less-safe-p numbers)))
