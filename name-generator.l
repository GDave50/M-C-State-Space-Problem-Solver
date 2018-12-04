; Modelling a name-generator
(defclass name-generator ()
  (
    (prefix :accessor name-generator-prefix :initarg :prefix :initform "name")
    (nr :accessor name-generator-nr :initform 0)
  )
)

(defmethod next ((ng name-generator))
  (setf (name-generator-nr ng) (+ 1 (name-generator-nr ng)))
  (concatenate 'string
    (name-generator-prefix ng)
    (write-to-string (name-generator-nr ng))
  )
)
