(defun read-disk (file)
  (with-open-file (s file)
    (let* ((sizes (loop with line = (read-line s)
                        for char across line
                        collecting (- (char-code char) 48)))
           (input (make-array (reduce #'+ sizes))))
      (loop
        with index = 0
        for range = 0 then (1+ range)
        for size in sizes
        for file = t then (not file)
        do (loop for i from 0 below size
                 do (progn
                      (setf (aref input index)(if file (floor (/ range 2)) -1))
                      (incf index)
                      )))
      input)))

(defun seek-next-empty (disk start)
  "index of closest empty block, starting from start. nil if not found"
  (loop for i from start below (length disk) while (> (aref disk i) -1) finally (return  (when (< i (length disk) ) i))))

(defun seek-prev-file (disk start)
  "index of closest file block, from right to left, starting from start. nil if not found"
  (loop for i from start downto 0 while (= (aref disk i) -1) finally (return (when (>= i 0) i))))

(seek-prev-file (read-disk "test1") 14)
(aref (read-disk "test1") 14)

(defun defrag (disk)
  "defrag in place"
  (loop
    with input = disk
    for left-cursor = (seek-next-empty input 0) then (seek-next-empty input left-cursor)
    for right-cursor = (seek-prev-file input (1- (length input))) then (seek-prev-file input right-cursor)
    while (and left-cursor right-cursor (< left-cursor right-cursor))
    do (let ((left-val (aref input left-cursor)))
         (setf (aref input left-cursor) (aref input right-cursor))
         (setf (aref input right-cursor) left-val))
    finally (return input)))

(defun checksum (disk)
  (loop for id across disk
        for i from 0 below (length disk)
        while (> id -1)
        summing (* i id)))

;; part1
(checksum  (defrag (read-input "input")))
