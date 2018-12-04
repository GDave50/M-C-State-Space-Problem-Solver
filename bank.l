; Modelling a bank
(defclass bank ()
  (
    (missionaries :accessor bank-missionaries :initarg :missionaries)
    (cannibals :accessor bank-cannibals :initarg :cannibals)
    (boat :accessor bank-boat :initarg :boat)
  )
)

(defmethod display ((b bank))
  (format t "(~A ~A ~A)~%"
    (bank-missionaries b)
    (bank-cannibals b)
    (bank-boat b)
  )
  nil
)

(defmethod to-string ((b bank))
  (format nil "(~A ~A ~A)"
    (bank-missionaries b)
    (bank-cannibals b)
    (bank-boat b)
  )
)

(defmethod equal-bank-p ((b1 bank) (b2 bank))
  (and
    (equal (bank-missionaries b1) (bank-missionaries b2))
    (equal (bank-cannibals b1) (bank-cannibals b2))
    (eq (bank-boat b1) (bank-boat b2))
  )
)

(defmethod copy-bank ((b bank))
  (make-instance 'bank
    :missionaries (bank-missionaries b)
    :cannibals (bank-cannibals b)
    :boat (bank-boat b)
  )
)
