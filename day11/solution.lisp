(defun sort-stones (stones)
  (sort stones (lambda (s1 s2) (< (car s1) (car s2)))))

(defun count-one (list) (sort-stones (mapcar (lambda (entry) (cons entry 1)) list)))

(defparameter *input* (count-one '(27 10647 103 9 0 5524 4594227 902936)))
(defparameter *test* (count-one '(125 17)))


(defun compress (stones)
  (let ((ht (make-hash-table)))
    (dolist (stone stones)
      (if (gethash (car stone) ht)
          (incf (gethash (car stone) ht) (cdr stone))
          (setf (gethash (car stone) ht) (cdr stone))))
    (loop for stone being the hash-keys of ht
          collecting (cons stone (gethash stone ht)))))

(defun even-halves (stone)
  (let ((rstone (write-to-string stone))) (zerop (mod (length rstone) 2))))

(defun halves (stone)
  (let* ((rstone (write-to-string stone))
         (halfpoint (/ (length rstone) 2)))
    (list (read-from-string (subseq rstone 0 halfpoint)) (read-from-string  (subseq rstone halfpoint)))))

(defun next-stones (stones)
  (compress (loop for stone in stones
                  appending (cond ((zerop (car stone)) (list (cons 1 (cdr stone))))
                                  ((even-halves (car stone))
                                   (mapcar
                                    (lambda (half) (cons half (cdr stone)))
                                    (halves (car stone))))
                                  (t (list (cons (* 2024 (car stone)) (cdr stone))))))))

(defun count-total (stones)
  (loop for stone in stones summing (cdr stone)))

;; part 1
(defun solve (stones iterations)
  (loop
    with known = (make-hash-table :test #'equal)
    for i from 0 upto iterations
    for step = *input* then (next-stones step)
    while (not (gethash step known))
    do (progn  (setf (gethash step known) t))
    finally (return (count-total step))))

;; part1
(solve *input* 25)
;; part2
(solve *input* 75)
