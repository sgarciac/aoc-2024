;;;; 2d matrix stuff
(defun rows (m) (array-dimension m 0))
(defun cols (m) (array-dimension m 1))
(defun row (pos) (car pos))
(defun col (pos) (cdr pos))
(defun make-pos (row col) (cons row col))
(defun apply-dir (pos dir) (cons (+ (car pos) (car dir)) (+ (cdr pos) (cdr dir))))
(defvar *up* (cons -1 0))
(defvar *left* (cons 0 -1))
(defvar *right* (cons 0 1))
(defvar *down* (cons 1 0))

(defun up (pos) (apply-dir pos *up*))
(defun down (pos) (apply-dir pos *down*))
(defun left (pos) (apply-dir pos *left*))
(defun right (pos) (apply-dir pos *right*))

(defun mval (m pos) (aref m (car pos) (cdr pos)))
(defun within-bounds (m pos)
  (and (>= (row pos) 0) (>= (col pos) 0)
       (< (row pos) (rows m)) (< (col pos) (cols m))))
(defun adjs (m pos)
  (loop for adj in (list (up pos) (down pos) (left pos) (right pos))
        when (within-bounds m adj) collect adj))
;;;; end 2d matrix stuff

;;; pad keys
(defvar *dirpad-near*)
(defvar *numpad-near*)
(defvar *dirkey-to-cell*)
(defvar *numkey-to-cell*)

(defun init-dirpad ()
  (setf *dirpad-near* (make-array '(2 3) :initial-element nil))
  (setf *dirkey-to-cell* (make-hash-table))
  (loop for row in '((nil :up :a) (:left :down :right))
        for r = 0 then (1+ r)
        do (loop for key in row
                 for c = 0 then (1+ c)
                 do (progn
                      (setf (aref *dirpad-near* r c) key)
                      (setf (gethash key *dirkey-to-cell*) (cons r c))
                      ))))

(defun dirkey-to-dir (key)
  (ecase key (:up *up*) (:down *down*) (:left *left*) (:right *right*)))

(defun init-numpad ()
  (setf *numpad-near* (make-array '(4 3) :initial-element nil))
  (setf *numkey-to-cell* (make-hash-table))
  (loop for row in '((:7 :8 :9) (:4 :5 :6) (:1 :2 :3) (nil :0 :a))
        for r = 0 then (1+ r)
        do (loop for key in row
                 for c = 0 then (1+ c)
                 do (progn
                      (setf (aref *numpad-near* r c) key)
                      (setf (gethash key *numkey-to-cell*) (cons r c))
                      ))))

(defun valid-in-dirpad (pos)
  (and (within-bounds *dirpad-near* pos)
       (aref *dirpad-near* (car pos) (cdr pos))))

(defun valid-in-numpad (pos)
  (and (within-bounds *numpad-near* pos)
       (aref *numpad-near* (car pos) (cdr pos))))

(init-dirpad)
(init-numpad)

(defun dirkey-to-cell (key)
  (gethash key *dirkey-to-cell*))

(defun numkey-to-cell (key)
  (gethash key *numkey-to-cell*))

(defun cell-to-dirkey (cell)
  (aref *dirpad-near* (car cell) (cdr cell)))

(defun cell-to-numkey (cell)
  (aref *numpad-near* (car cell) (cdr cell)))

(defun dir-close (key)
  "keys next to key in the directional pad"
  (mapcar (lambda (cell) (aref *dirpad-near* (row cell) (col cell)))
          (adjs *dirpad-near* (dirkey-to-cell key))))

(defun num-close (key)
  "keys next to key in the numerical pad"
  (mapcar (lambda (cell) (aref *numpad-near* (row cell) (col cell)))
          (adjs *numpad-near* (numkey-to-cell key))))
;; end pad keys

;; graph of possible actions

(defstruct node
  dirkeys ;; array of direction pad keys
  numkey  ;; numeric pad key
  )

(defun init-position (n)
  "creates initial position with n direction pads"
  (make-node
   :dirkeys (make-array (list n) :initial-element :a)
   :numkey :a))

(defun node-init-numkey (key n)
  (let ((node (init-position n)))
    (setf (node-numkey node) key)
    node))

(defun node-dirkey (node offset)
  (aref (node-dirkeys node) offset))

(defun move (node dir offset)
  "return next node after move, if move is legit"
  (if (< offset (length (node-dirkeys node)))
      ;; move the dirpad
      (when (valid-in-dirpad (apply-dir (dirkey-to-cell (node-dirkey node offset)) dir))
        (let ((dirkeys-copy (copy-seq (node-dirkeys node))))
          (setf (aref dirkeys-copy offset) (cell-to-dirkey (apply-dir (dirkey-to-cell (node-dirkey node offset)) dir)))
          (make-node
           :dirkeys dirkeys-copy
           :numkey (node-numkey node))))
      ;; move the numpad if possible
      (when (valid-in-numpad (apply-dir (numkey-to-cell (node-numkey node)) dir))
        (make-node
         :dirkeys (node-dirkeys node)
         :numkey (cell-to-numkey (apply-dir (numkey-to-cell (node-numkey node)) dir))))))

(defun move-human (node dir)
  (move node dir 0))

(defun push-button (node offset)
  "return next node after pushing a button, if new position is legit"
  (if (< offset (length (node-dirkeys node)))
      ;; push a dirpad
      (if (eq (node-dirkey node offset) :a)
          ;; push next button
          (push-button node (1+ offset))
          ;; otherwise, move next
          (move node (dirkey-to-dir (node-dirkey node offset)) (1+ offset)))
      ;; push the numpad: it doesnt change the node
      node))

(defun push-button-human (node)
  (push-button node 0))

(defun valid-next-nodes (node)
  (remove node
          (remove nil (loop for key in (list :up :down :right :left :a)
                            collect (if (eq key :a)
                                        (push-button-human node)
                                        (move-human node (dirkey-to-dir key)))))
          :test #'equalp))

;; dijkstra
(defstruct state visited unvisited)

(defun init-state (node)
  (let ((state (make-state
                :unvisited (make-hash-table :test #'equalp)
                :visited (make-hash-table :test #'equalp)
                )))
    (setf (gethash node (state-unvisited state)) 0)
    state))

(defun valid-next-nodes-non-visited (node visited)
  (remove-if (lambda (n) (gethash n visited))
             (remove node
                     (remove nil
                             (loop for key in (list :up :down :right :left :a)
                                   collect (if (eq key :a)
                                               (push-button-human node)
                                               (move-human node (dirkey-to-dir key)))))
                     :test #'equalp)))

(defun closest-unvisited (state)
  (loop
    with closest = nil
    for nodeh being the hash-keys in (state-unvisited state) using (hash-value distance)
    do (when (or (not closest)
                 (< distance
                    (gethash closest (state-unvisited state))))
         (setf closest nodeh))
    finally (return closest)))


(defun dijkstra (start end)
  (loop
    with state = (init-state start)
    for closest = (closest-unvisited state)
    while closest
    do (let* ((adjs (valid-next-nodes-non-visited closest (state-visited state))))
         (when (equalp closest end) (return-from dijkstra (gethash closest (state-unvisited state))))
         (loop for adj in adjs
               do (when (or (not (gethash adj (state-unvisited state)))
                            (< (1+ (gethash closest (state-unvisited state)))
                               (gethash adj (state-unvisited state))))
                    (setf (gethash adj (state-unvisited state)) (1+ (gethash closest (state-unvisited state))))))
         (setf (gethash closest (state-visited state)) (gethash closest (state-unvisited state)))
         (remhash closest (state-unvisited state)))
    finally (return (state-visited state))))

(defun part1 (k1 k2 k3 k4)
  (+ 4 (dijkstra (node-init-numkey :a 2) (node-init-numkey k1 2))
     (dijkstra (node-init-numkey k1 2) (node-init-numkey k2 2))
     (dijkstra (node-init-numkey k2 2) (node-init-numkey k3 2))
     (dijkstra (node-init-numkey k3 2) (node-init-numkey k4 2))))

(+ (* 964  (part1 :9 :6 :4 :a))
   (* 246 (part1 :2 :4 :6 :a))
   (* 973 (part1 :9 :7 :3 :a))
   (* 682 (part1 :6 :8 :2 :a))
   (* 180 (part1 :1 :8 :0 :a)))


(dijkstra (node-init-numkey :a 25) (node-init-numkey :0 25))

(defun part2 (k1 k2 k3 k4)
  (+ 4 (gethash (node-numkey k1 2)
                )
     (gethash (node-numkey k2 2) (dijkstra (node-numkey k1 2)))
     (gethash (node-numkey k3 2) (dijkstra (node-numkey k2 2)))
     (gethash (node-numkey k4 2) (dijkstra (node-numkey k3 2)))))

(+ (* 964  (part1 :9 :6 :4 :a))
   (* 246 (part1 :2 :4 :6 :a))
   (* 973 (part1 :9 :7 :3 :a))
   (* 682 (part1 :6 :8 :2 :a))
   (* 180 (part1 :1 :8 :0 :a)))


(loop for (a b) on (list 1 2 3 4) by #'cdr do (print (cons a b)))



























(defun move-human-list (keys n)
  (loop
    for node = (init-position n) then (if (eq key :a)
                                          (push-button-human node)
                                          (move-human node (dirkey-to-dir key)))
    for key in keys
    do (print node)
    finally (print node)))

(move-human-list (list :down :left :left :a :right :right :up :a :left :a :right :a) 2)
