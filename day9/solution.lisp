;; part1
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
  (loop for i from start below (length disk) while (> (aref disk i) -1) finally (return (when (< i (length disk)) i))))

(defun seek-prev-file (disk start)
  "index of closest file block, from right to left, starting from start. nil if not found"
  (loop for i from start downto 0 while (= (aref disk i) -1) finally (return (when (>= i 0) i))))

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
        when (> id -1) summing (* i id)))

(checksum  (defrag (read-disk "input")))

;; part2
(defun find-id (id disk)
  "find first index of file"
  (loop for start from 0 below (length disk) while (not (= (aref disk start) id)) finally (return start)))

(defun count-id (disk start id)
  (loop for index from start below (length disk) while (= (aref disk index) id) finally (return (- index start))))

(defun seek-next-empty-opt (disk start end)
  "index of closest empty block, starting from start and no further than end. nil if not found"
  (loop for i from start upto end while (> (aref disk i) -1)
        finally (return (when (< i end) i))))

(defun count-empty (disk start) (count-id disk start -1))

(defun find-empty (disk minsize end)
  (loop
    with index = 0
    for empty = (seek-next-empty-opt disk index end)
    while empty
    do (let ((ce (count-empty disk empty)))
         (if (< ce minsize)
             (incf index ce)
             (return-from find-empty empty)))))


(defun extract-ids (disk)
  (remove-duplicates (loop for val across disk when (> val -1) collect val)))

(let* ((disk (read-disk "input"))
       (ids (extract-ids disk)))
  (loop for id in (reverse  ids)
        do (let* ((id-start (find-id id disk))
                  (size-id (count-id disk id-start id))
                  (empty-start (find-empty disk size-id id-start)))
             (when (zerop (mod id 200)) (format t "id:~A id-start:~A size-id:~A~%" id id-start size-id))
             (when (and empty-start (< empty-start id-start))
               (loop for i from 0 below size-id
                     do (setf (aref disk (+ empty-start i)) id
                              (aref disk (+ id-start i)) -1)))
             ))
  (print (checksum disk)))
