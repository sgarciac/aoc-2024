(defun read-input (file)
  (with-open-file (s file)
    (loop for number = (read s nil)
          while number collect number)))

(defun mix (secret value)
  (logxor value secret))

(defun prune (secret)
  (mod secret 16777216))

(defun next (secret)
  (let* (
         ;; first step
         (64ed (* 64 secret))
         (first-step (prune (mix secret 64ed)))
         ;; second step
         (32ed (floor (/ first-step 32)))
         (second-step (prune (mix first-step 32ed)))
         ;;
         (2048ed (* second-step 2048))
         (third-step (prune (mix second-step 2048ed))))
    third-step))

(defun next-n (secret n)
  (let ((s secret))
    (dotimes (i n)
      (setf s (next s)))
    s))

;; part1
(loop for entry in (read-input "input") summing (next-n entry 2000))

;; part 2
(defun generate-n (secret n)
  (loop
    for i from 0 upto n
    for s = secret then (next s)
    collecting (mod s 10)))

(defun diffs (entries)
  (loop for (a b) on entries by #'cdr
        when (and a b) collecting (- b a)))

(defun quartets (secret n)
  (let* ((vs (generate-n secret n))
         (ds (diffs vs)))
    (loop for (a b c d) on (cdr vs)
          for (da db dc dd) on ds
          when (and a b c d) collecting (cons (list da db dc dd) d))))

(defun first-quartets (quartets)
  (loop with fqs = (make-hash-table :test #'equal)
        for q in quartets
        when (not (gethash (car q) fqs))
          do (setf (gethash (car q) fqs) (cdr q))
        finally (return fqs)))

(defun part2 (secrets)
  (let ((fqs (mapcar (lambda (secret) (first-quartets (quartets secret 2000))) secrets)))
    (labels ((total (quartet) (loop for fq in fqs when (gethash quartet fq) summing (gethash quartet fq))))
      (loop for i from -9 upto 9
            maximizing
            (loop for j from -9 upto 9
                  maximizing
                  (loop for k from -9 upto 9
                        maximizing
                        (loop for l from -9 upto 9
                              maximizing (total (list i j k l)))))))))
