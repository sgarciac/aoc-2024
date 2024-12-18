(defstruct computer
  pc
  instructions
  a b c
  )

(defstruct instruction
  opcode
  operand)

(defun mi (opcode operand)
  (make-instruction :opcode opcode :operand operand))

(defun to-instructions (&rest vals)
  (loop for (opcode operand) on vals by #'cddr
        for count = 1 then (1+ count)
        collecting (mi opcode operand) into instructions
        finally (return (make-array (list count) :initial-contents instructions))))

(defun init-computer (a)
  (make-computer :pc 0
                 :a a
                 :b 0
                 :c 0
                 :instructions (to-instructions 2 4 1 3 7 5 0 3 4 3 1 5 5 5 3 0)))

(defun combo-val (operand c)
  (cond ((< operand 4) operand)
        ((= operand 4) (computer-a c))
        ((= operand 5) (computer-b c))
        ((= operand 6) (computer-c c))))

(defun current-instruction (c)
  (aref (computer-instructions c) (computer-pc c)))

(defun finished (c)
  (>= (computer-pc c) (length (computer-instructions c))))

(defun exec (c)
  (let ((opcode (instruction-opcode (current-instruction c)))
        (operand (instruction-operand (current-instruction c))))
    (case opcode
      (0 ;; adv
       (setf (computer-a c) (truncate (/ (computer-a c) (expt 2 (combo-val operand c)))))
       (incf (computer-pc c))
       nil)
      (1 ;; bxl
       (setf (computer-b c) (logxor (computer-b c) operand))
       (incf (computer-pc c))
       nil)
      (2 ;; bst
       (setf (computer-b c) (mod (combo-val operand c) 8))
       (incf (computer-pc c))
       nil
       )
      (3 ;; jnz
       (setf (computer-pc c)
             (if (zerop (computer-a c))
                 (1+ (computer-pc c))
                 operand))
       nil)
      (4 ;; bxc
       (setf (computer-b c) (logxor (computer-b c) (computer-c c)))
       (incf (computer-pc c))
       nil)
      (5 ;; output
       (incf (computer-pc c))
       (mod (combo-val operand c) 8)
       )
      (6 ;; bdv
       (setf (computer-b c) (truncate (/ (computer-a c) (expt 2 (combo-val operand c)))))
       (incf (computer-pc c))
       nil)
      (7 ;; cdv
       (setf (computer-c c) (truncate (/ (computer-a c) (expt 2 (combo-val operand c)))))
       (incf (computer-pc c))
       nil)
      )))



;; part 1
(loop with c = (init-computer 22571680)
      for output = (exec c)
      while (not (finished c))
      when output collecting output)

;; part 2
(defun computer-signature (c)
  (list (computer-pc c) (computer-a c) (computer-b c) (computer-c c)))

(defun valid-prefix (program prefix-source n)
  (if (< (length program) (length prefix))
      nil
      (loop for p in prefix
            for b in program
            for
            do (when (not (= p b))
                 (return-from valid-prefix nil))
            finally (return t))))

(defun exec-til-end (c)
  (loop
    with visited = (make-hash-table :test #'equal)
    for output = (exec c)
    while (and (not (gethash (computer-signature c) visited)) (not (finished c)))
    do (setf (gethash (computer-signature c) visited) t)
    when output collecting output into total-output
      finally (return total-output)))

(defun find-solutions (program total power)
  (dotimes (i 8)
    (let* ((new-total (+ total (* i (expt 8 power))))
           (solution (exec-til-end (init-computer new-total))))
      (if (equal program solution)
          (format t "~A ~A~%" solution new-total)
          (when (and (= (length program) (length solution)) (equal (subseq program power) (subseq solution power)))
            (find-solutions program new-total (1- power)))))))

(find-solutions (list 2 4 1 3 7 5 0 3 4 3 1 5 5 5 3 0) 0 15)
