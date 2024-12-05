(ql:quickload "split-sequence")

(defvar *rules*)
(defvar *manuals*)

(defun load-input (file)
  (setf *rules* (make-array '(100 100) :initial-element nil))
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while (not (string= line ""))
          do (let ((parts (split-sequence:split-sequence #\| line)))
               (setf (aref *rules*
                           (parse-integer (first parts))
                           (parse-integer (second parts))) t)))
    (setf *manuals*
          (loop for line = (read-line s nil)
                while line
                collecting (mapcar #'parse-integer (split-sequence:split-sequence #\, line))))))

(defun rule-ok (page1 page2) (not (aref *rules* page2 page1)))

(defun check-order (manual)
  (or (not (cadr manual))
      (and (rule-ok (car manual) (cadr manual))
           (check-order (cdr manual)))))

(defun middle (list)
  (nth (floor (/ (length list) 2)) list))

(load-input "input")

;; part 1
(loop for manual in *manuals*
      when (check-order manual) sum (middle manual))

;; part 2
(loop for manual in *manuals*
      when (not (check-order manual))
        sum (let ((sorted (sort manual #'rule-ok)))
              (middle sorted)))
