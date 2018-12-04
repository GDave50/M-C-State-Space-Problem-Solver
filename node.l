; Modelling a node
(defclass node ()
  (
    (name :accessor node-name :initarg :name)
    (state :accessor node-state :initarg :state)
    (parent :accessor node-parent :initarg :parent)
    (operator :accessor node-operator :initarg :operator)
  )
)

(defmethod display ((n node))
  (format t "~A~%" (to-string (node-state n)))
)

(defmethod exploredp ((e-node node))
  (member-node-p e-node *explored*)
)

(defmethod member-node-p ((n node) (l list))
  (cond
    ((null l)
      nil
    )
    ((equal-state-p (node-state n) (node-state (car l)))
      t
    )
    (t
      (member-node-p n (cdr l))
    )
  )
)

(defmethod rootp ((n node))
  (equal (node-name n) "root")
)

(defmethod children-of ((e-node node) &aux kids state)
  (setf state (node-state e-node))
  (setf kids '())
  (if (applicablep *MR* state)
    (push (child-of e-node *MR*) kids))
  (if (applicablep *MMR* state)
    (push (child-of e-node *MMR*) kids))
  (if (applicablep *CR* state)
    (push (child-of e-node *CR*) kids))
  (if (applicablep *CCR* state)
    (push (child-of e-node *CCR*) kids))
  (if (applicablep *MCR* state)
    (push (child-of e-node *MCR*) kids))
  (if (applicablep *ML* state)
    (push (child-of e-node *ML*) kids))
  (if (applicablep *MML* state)
    (push (child-of e-node *MML*) kids))
  (if (applicablep *CL* state)
    (push (child-of e-node *CL*) kids))
  (if (applicablep *CCL* state)
    (push (child-of e-node *CCL*) kids))
  (if (applicablep *MCL* state)
    (push (child-of e-node *MCL*) kids))
  kids
)
