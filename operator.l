; Modelling an operator
(defclass operator ()
  (
    (name :accessor operator-name :initarg :name)
    (precondition :accessor operator-precondition :initarg :precondition)
    (description :accessor operator-description :initarg :description)
  )
)

(defmethod display ((op operator))
  (format t "~A~%"
    (operator-description op)
  )
  nil
)

(defmethod establish-operators ()
  (setf *MR*
    (make-instance 'operator
      :name 'MR
      :precondition "For LB: B and M > 0 and M-1 >= C and 3-M+1 >= 3-C"
      :description "One missionary goes to right bank"
    )
  )
  (setf *MMR*
    (make-instance 'operator
      :name 'MMR
      :precondition "For LB: B and M > 1 and M-2 >= C and 3-M+2 >= 3-C"
      :description "Two missionaries go to right bank"
    )
  )
  (setf *CR*
    (make-instance 'operator
      :name 'CR
      :precondition "For LB: B and C > 0 and (3-M > 0 --> 3-M >= 3-C+1)"
      :description "One cannibal goes to right bank"
    )
  )
  (setf *CCR*
    (make-instance 'operator
      :name 'CCR
      :precondition "For LB: B and C > 1 and (3-M > 0 --> 3-M >= 3-C+2)"
      :description "Two cannibals go to right bank"
    )
  )
  (setf *MCR*
    (make-instance 'operator
      :name 'MCR
      :precondition "For LB: B and C > 0 and M > 0 and 3-M+1 >= 3-C+1"
      :description "One cannibal and one missionary go to right bank"
    )
  )
  (setf *ML*
    (make-instance 'operator
      :name 'ML
      :precondition "For RB: B and M > 0 and M-1 >= C and 3-M+1 >= 3-C"
      :description "One missionary goes to left bank"
    )
  )
  (setf *MML*
    (make-instance 'operator
      :name 'MML
      :precondition "For RB: B and M > 1 and M-2 >= C and 3-M+2 >= 3-C"
      :description "Two missionaries go to left bank"
    )
  )
  (setf *CL*
    (make-instance 'operator
      :name 'CL
      :precondition "For RB: B and C > 0 and (3-M > 0 --> 3-M >= 3-C+1)"
      :description "One cannibal goes to left bank"
    )
  )
  (setf *CCL*
    (make-instance 'operator
      :name 'CCL
      :precondition "For RB: B and C > 1 and (3-M > 0 --> 3-M >= 3-C+2)"
      :description "Two cannibals go to left bank"
    )
  )
  (setf *MCL*
    (make-instance 'operator
      :name 'MCL
      :precondition "For RB: B and C > 0 and M > 0 and 3-M+1 >= 3-C+1"
      :description "One cannibal and one missionary go to left bank"
    )
  )
)
