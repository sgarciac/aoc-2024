(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")

(defun read-input (file)
  (with-open-file  (s file)
    (loop for line = (read-line s nil)
          while line
          collect (cl-ppcre:register-groups-bind (c1 c2) ("(..)-(..)" line)
                    (cons c1 c2)))))

(defun unique-computers (input)
  (remove-duplicates (loop for entry in input appending (list (car entry) (cdr entry))) :test #'string=))

(defun computers-to-integers (computers)
  (loop
    with ht = (make-hash-table :test #'equal)
    for i = 0 then (1+ i)
    for computer in computers do (setf (gethash computer ht) i)
    finally (return ht)))

(defun process-input (file)
  (let* ((input (read-input "input"))
         (computers (unique-computers input))
         (ctoi (computers-to-integers computers))
         (cnxs (make-array (list (length computers) (length computers)) :initial-element nil)))
    (dolist (cxn input)
      (setf (aref cnxs (gethash (car cxn) ctoi) (gethash (cdr cxn) ctoi)) t)
      (setf (aref cnxs (gethash (cdr cxn) ctoi) (gethash (car cxn) ctoi)) t))
    cnxs))

;; part1
(defun part1 (file)
  (let* ((input (read-input file))
         (computers (unique-computers input))
         (ctoi (computers-to-integers computers))
         (cnxs (make-array (list (length computers) (length computers)) :initial-element nil)))
    (dolist (cxn input)
      (setf (aref cnxs (gethash (car cxn) ctoi) (gethash (cdr cxn) ctoi)) t)
      (setf (aref cnxs (gethash (cdr cxn) ctoi) (gethash (car cxn) ctoi)) t))
    (let ((result '()))
      (alexandria:map-combinations (lambda (triad)
                                     (when (and (find-if (lambda (c) (char= (aref c 0) #\t)) triad)
                                                (aref cnxs (gethash (first triad) ctoi) (gethash (second triad) ctoi))
                                                (aref cnxs (gethash (first triad) ctoi) (gethash (third triad) ctoi))
                                                (aref cnxs (gethash (second triad) ctoi) (gethash (third triad) ctoi)))
                                       (push triad result))) computers :length 3)
      (length result))))

(part1 "input")

;; part2
(defun extend-set (s length cnxs)
  (loop for i from 0 below length
        when (and
              (not (find i s))
              (every (lambda (entry) (aref cnxs entry i)) s))
          collecting (sort (cons i (copy-seq s)) #'<)))

(defun extend-sets (sets length cnxs)
  (remove-duplicates (loop for set in sets
                           appending (extend-set set length cnxs))
                     :test #'equal))


(defun part2 (file)
  (let* ((input (read-input file))
         (computers (unique-computers input))
         (computers-count (length computers))
         (ctoi (computers-to-integers computers))
         (cnxs (make-array (list computers-count computers-count) :initial-element nil)))
    (dolist (cxn input)
      (setf (aref cnxs (gethash (car cxn) ctoi) (gethash (cdr cxn) ctoi)) t)
      (setf (aref cnxs (gethash (cdr cxn) ctoi) (gethash (car cxn) ctoi)) t))
    (let ((triads '()))
      (alexandria:map-combinations (lambda (triad)
                                     (when (and (find-if (lambda (c) (char= (aref c 0) #\t)) triad)
                                                (aref cnxs (gethash (first triad) ctoi) (gethash (second triad) ctoi))
                                                (aref cnxs (gethash (first triad) ctoi) (gethash (third triad) ctoi))
                                                (aref cnxs (gethash (second triad) ctoi) (gethash (third triad) ctoi)))
                                       (push (sort  (list (gethash (first triad) ctoi) (gethash (second triad) ctoi) (gethash (third triad) ctoi) )
                                                    #'<
                                                    ) triads))) computers :length 3)
      (let ((ids (first (loop for current = triads then (extend-sets current computers-count cnxs)
                              while (> (length current) 1)
                              finally (return current)))))
        (sort (loop for c in computers when (find (gethash c ctoi) ids) collect c)
              #'string<
              )))))

(part2 "input")
