;; part 1: a shell grep that I didnt kept.

;; part 2
(ql:quickload "cl-ppcre")

(defstruct input
  parts
  entries)

(defun starts-with-p (start s)
  (let ((start-length (length (string start))))
    (when (>= (length s) start-length)
      (string= s start :start1 0 :end1 start-length))))

(defun read-input (file)
  (with-open-file (s file)
    (make-input
     :parts (cl-ppcre:split ", " (read-line s))
     :entries (cdr (loop for line = (read-line s nil) while line collecting line)))))

(defun count-solutions (target parts memo)
  (if (gethash target memo)
      (gethash target memo)
      (setf (gethash target memo)
            (loop for part in parts
                  summing (cond ((string= target part) 1)
                                ((starts-with-p part target) (count-solutions (subseq target (length part)) parts memo))
                                (t 0)))) ))

(let (
      (memo (make-hash-table :test #'equal))
      (input (read-input "input")))
  (loop for entry in (input-entries input)
        summing (count-solutions entry (input-parts input) memo)))
