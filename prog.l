; Program Description
;
; Missionaries and Cannibals problem solver.
;
; This program is a state space problem solver for the classic
;   "Missionaries and Cannibals" problem. An explicit state
;   space tree is grown in concert with breadth-first search
;   for a solution
;
; Banks are represented as a 3-slot class consisting of
;   missionaries, cannibals and boat.
;
; States are represented as a 2-slot class consisting of
;   left-bank and rigt-bank.
;
; Operators are represented as a 3-slot class consisting of a 
;   name, a precondition and a description.
;
; Nodes are represented as a 4-slot class consisting of
;   a state, a parent node, a move (state space operator)
;   and a name.

(load "bank.l")
(load "name-generator.l")
(load "node.l")
(load "operator.l")
(load "state.l")

; Main method
(defmethod mc ()
  (establish-operators)
  (setup)
  (solve)
)

; The setup
(defmethod setup (&aux root lb rb istate)
  ; establish root node
  (setf lb (make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b))
  (setf rb (make-instance 'bank :missionaries '() :cannibals '() :boat nil))
  (setf istate (make-instance 'state :left-bank lb :right-bank rb))
  (setf root (make-instance 'node :state istate :name "root"))
  
  ; initialize list of unexplored nodes
  (setf *unexplored* (list root))
  
  ; initialize list of explored nodes
  (setf *explored* '())
  
  ; get ready to create good names
  (setf *ng* (make-instance 'name-generator :prefix "N"))
)

; Breadth-first search
(defmethod solve (&aux kids e-node)
  (if *trace-search*
    (let ()
      (terpri) (write-line ">>> Solve") (terpri)
      (display-explored-nodes)
      (display-unexplored-nodes)
      (terpri)
    )
  )
  (cond
    ((null *unexplored*)
      (write-line "There is no solution.")
      (return-from solve nil)
    )
  )
  (setf e-node (pop *unexplored*))
  (if *trace-search*
    (let ()
      (princ "Pop ")
      (display e-node)
    )
  )
  (cond
    ((goalp (node-state e-node))
      (format t "~%~A~%" "Solution:")
      (display-solution e-node)
    )
    ((exploredp e-node)
      (solve)
    )
    (t
      (push e-node *explored*)
      (setf kids (children-of e-node))
      (setf *unexplored* (append *unexplored* kids))
      (solve)
    )
  )
  nil
)

; Generating children

(defmethod child-of ((n node) (op operator) &aux c)
  (setf new-node (make-instance 'node))
  (setf (node-name new-node) (next *ng*))
  (setf (node-parent new-node) n)
  (setf (node-operator new-node) op)
  (setf c (copy-state (node-state n)))
  (apply-operator op c)
  (setf (node-state new-node) c)
  new-node
)

(setf *trace-search* t)

(defmethod display-explored-nodes ()
  (princ "EXPLORED") (terpri)
  (mapcar #'display *explored*)
  (terpri)
  nil
)

(defmethod display-unexplored-nodes ()
  (princ "UNEXPLORED") (terpri)
  (mapcar #'display *unexplored*)
  (terpri)
  nil
)

(defmethod applicablep ((op operator) (s state))
  (cond
    ((eq (operator-name op) 'MR)
      (applicable-mr s)
    )
    ((eq (operator-name op) 'MMR)
      (applicable-mmr s)
    )
    ((eq (operator-name op) 'CR)
      (applicable-cr s)
    )
    ((eq (operator-name op) 'CCR)
      (applicable-ccr s)
    )
    ((eq (operator-name op) 'MCR)
      (applicable-mcr s)
    )
    ((eq (operator-name op) 'ML)
      (applicable-ml s)
    )
    ((eq (operator-name op) 'MML)
      (applicable-mml s)
    )
    ((eq (operator-name op) 'CL)
      (applicable-cl s)
    )
    ((eq (operator-name op) 'CCL)
      (applicable-ccl s)
    )
    ((eq (operator-name op) 'MCL)
      (applicable-mcl s)
    )
  )
)

(defmethod apply-operator ((op operator) (s state))
  (cond
    ((eq (operator-name op) 'MR)
      (mr s)
      (br s)
    )
    ((eq (operator-name op) 'MMR)
      (mr s)
      (mr s)
      (br s)
    )
    ((eq (operator-name op) 'CR)
      (cr s)
      (br s)
    )
    ((eq (operator-name op) 'CCR)
      (cr s)
      (cr s)
      (br s)
    )
    ((eq (operator-name op) 'MCR)
      (mr s)
      (cr s)
      (br s)
    )
    ((eq (operator-name op) 'ML)
      (ml s)
      (bl s)
    )
    ((eq (operator-name op) 'MML)
      (ml s)
      (ml s)
      (bl s)
    )
    ((eq (operator-name op) 'CL)
      (cl s)
      (bl s)
    )
    ((eq (operator-name op) 'CCL)
      (cl s)
      (cl s)
      (bl s)
    )
    ((eq (operator-name op) 'MCL)
      (ml s)
      (cl s)
      (bl s)
    )
  )
)

(defmethod display-solution ((n node) &aux description)
  (if (not (rootp n))
    (let ()
      (setf description (operator-description (node-operator n)))
      (display-solution (node-parent n))
      (format t "~A~%" description)
    )
  )
)
