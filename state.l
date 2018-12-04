; Modelling a state
(defclass state ()
  (
    (left-bank :accessor state-left-bank :initarg :left-bank)
    (right-bank :accessor state-right-bank :initarg :right-bank)
  )
)

(defmethod display ((s state))
  (display (state-left-bank s))
  (display (state-right-bank s))
  nil
)

(defmethod to-string ((s state))
  (format nil "~A ~A"
    (to-string (state-left-bank s))
    (to-string (state-right-bank s))
  )
)

(defmethod equal-state-p ((s1 state) (s2 state))
  (and
    (equal-bank-p (state-left-bank s1) (state-left-bank s2))
    (equal-bank-p (state-right-bank s1) (state-right-bank s2))
  )
)

(defmethod goalp ((s state) &aux rb)
  (setf rb (state-right-bank s))
  (and
    (= (length (bank-missionaries rb)) 3)
    (= (length (bank-cannibals rb)) 3)
    (eq (bank-boat rb) 'b)
  )
)

(defmethod applicable-mr ((s state) &aux bank m c)
  (setf bank (state-left-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= m 1)
    (if (> (- m 1) 0) (>= (- m 1) c) t)
    (>= (+ (- 3 m) 1) (- 3 c))
  )
)

(defmethod applicable-mmr ((s state) &aux bank m c)
  (setf bank (state-left-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= m 2)
    (if (> (- m 2) 0) (>= (- m 2) c) t)
    (>= (+ (- 3 m) 2) (- 3 c))
  )
)

(defmethod applicable-cr ((s state) &aux bank m c)
  (setf bank (state-left-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= c 1)
    (if (> (- 3 m) 0) (>= (- 3 m) (+ (- 3 c) 1)) t)
  )
)

(defmethod applicable-ccr ((s state) &aux bank m c)
  (setf bank (state-left-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= c 2)
    (if (> (- 3 m) 0) (>= (- 3 m) (+ (- 3 c) 2)) t)
  )
)

(defmethod applicable-mcr ((s state) &aux bank m c)
  (setf bank (state-left-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= c 1)
    (>= m 1)
    (>= (+ (- 3 m) 1) (+ (- 3 c) 1))
  )
)

(defmethod applicable-ml ((s state) &aux bank m c)
  (setf bank (state-right-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= m 1)
    (if (> (- m 1) 0) (>= (- m 1) c) t)
    (>= (+ (- 3 m) 1) (- 3 c))
  )
)

(defmethod applicable-mml ((s state) &aux bank m c)
  (setf bank (state-right-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= m 2)
    (if (> (- m 2) 0) (>= (- m 2) c) t)
    (>= (+ (- 3 m) 2) (- 3 c))
  )
)

(defmethod applicable-cl ((s state) &aux bank m c)
  (setf bank (state-right-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= c 1)
    (if (> (- 3 m) 0) (>= (- 3 m) (+ (- 3 c) 1)) t)
  )
)

(defmethod applicable-ccl ((s state) &aux bank m c)
  (setf bank (state-right-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= c 2)
    (if (> (- 3 m) 0) (>= (- 3 m) (+ (- 3 c) 2)) t)
  )
)

(defmethod applicable-mcl ((s state) &aux bank m c)
  (setf bank (state-right-bank s))
  (setf m (length (bank-missionaries bank)))
  (setf c (length (bank-cannibals bank)))
  (and
    (eq (bank-boat bank) 'b)
    (>= c 1)
    (>= m 1)
    (>= (+ (- 3 m) 1) (+ (- 3 c) 1))
  )
)

(defmethod mr ((s state))
  (setf (bank-missionaries (state-left-bank s))
    (remove 'M (bank-missionaries (state-left-bank s)) :count 1))
  (setf (bank-missionaries (state-right-bank s))
    (cons 'M (bank-missionaries (state-right-bank s))))
)

(defmethod cr ((s state))
  (setf (bank-cannibals (state-left-bank s))
    (remove 'C (bank-cannibals (state-left-bank s)) :count 1))
  (setf (bank-cannibals (state-right-bank s))
    (cons 'C (bank-cannibals (state-right-bank s))))
)

(defmethod ml ((s state))
  (setf (bank-missionaries (state-right-bank s))
    (remove 'M (bank-missionaries (state-right-bank s)) :count 1))
  (setf (bank-missionaries (state-left-bank s))
    (cons 'M (bank-missionaries (state-left-bank s))))
)

(defmethod cl ((s state))
  (setf (bank-cannibals (state-right-bank s))
    (remove 'C (bank-cannibals (state-right-bank s)) :count 1))
  (setf (bank-cannibals (state-left-bank s))
    (cons 'C (bank-cannibals (state-left-bank s))))
)

(defmethod br ((s state))
  (setf (bank-boat (state-left-bank s)) nil)
  (setf (bank-boat (state-right-bank s)) 'B)
)

(defmethod bl ((s state))
  (setf (bank-boat (state-left-bank s)) 'B)
  (setf (bank-boat (state-right-bank s)) nil)
)

(defmethod copy-state ((s state))
  (make-instance 'state
    :left-bank (copy-bank (state-left-bank s))
    :right-bank (copy-bank (state-right-bank s))
  )
)