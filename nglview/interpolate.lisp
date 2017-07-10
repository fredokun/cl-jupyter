(in-package :nglv)

(defun lerp (a b t)
  (+ (* (- b a) t) a))

(defun linear (index t traj &optional (step 1))
  (let ((c #|help|# ) (cp #|help|#) (coords (lerp cp c t)))
    coords))
  
