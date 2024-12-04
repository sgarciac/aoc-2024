(ql:quickload "cl-ppcre")
(defvar *input* nil)
(defvar *rows* nil)
(defvar *cols* nil)

(defun load-input ()
  (with-open-file (stream "input")
    (loop
      for line = (read-line stream nil) while line
      collecting line into lines
      finally (progn
                (setf *rows* (length lines))
                (setf *cols* (length (first lines)))
                (setf *input* (make-array *rows* :initial-contents lines))))))

(defun ichar (row col)
  (aref (aref *input* row) col))

(defun in-bounds-p (row col)
  (and (>= row 0) (< row *rows*) (>= col 0) (< col *cols*)))

(defun get-seq (irow icol direction)
  (loop for row = irow then (+ row (car direction))
        for col = icol then (+ col (cdr direction))
        while (in-bounds-p row col)
        collecting (ichar row col) into list
        finally (return (coerce list 'string))))

(defun get-all-seqs ()
  (append
   ;; horizontal
   (loop for row from 0 below *rows*  collecting (get-seq row 0 (cons 0 1)))
   ;; vertical
   (loop for col from 0 below *cols* collecting (get-seq 0 col (cons 1 0)))
   ;; corner bottom left
   (loop for row from 0 below *rows* collecting (get-seq row 0 (cons 1 1)))
   ;; corner top right
   (loop for col from 1 below *cols* collecting (get-seq 0 col (cons 1 1)))
   ;; corner top left
   (loop for row from 0 below *rows* collecting (get-seq row 0 (cons -1 1)))
   ;; diagonal bottom right
   (loop for col from 1 below *cols* collecting (get-seq (1- *rows*) col (cons -1 1)))
   ))

;; part 1
(load-input)

(loop for line in (get-all-seqs)
      summing (+ (cl-ppcre:count-matches "XMAS" line)
                 (cl-ppcre:count-matches "XMAS" (reverse line))))

;; part 2
(defun get-xword (row col)
  (format nil "~A~A~A~A~A"
          (ichar row col) (ichar (1+  row) (1+ col)) (ichar (+ 2  row) (+ 2  col))
          (ichar (+ 2 row) col) (ichar row (+ 2 col))))

(loop for row from 0 below (- *rows* 2)
      summing (loop for col from 0 below (- *cols* 2)
                    counting (find (get-xword row col) '("SAMSM" "SAMMS" "MASSM" "MASMS") :test #'string=)))
