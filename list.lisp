
(in-package :monads)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LIST MONAD

;; (I'll use normal CL lists)

;; This is amazingly simple and straightforward. It allows us to do list comprehensions trivially
;; I could use this to define bind if I wanted
(defmethod join ((self list))
  (reduce #'append self))

(defmethod bind ((m list) f)
  (reduce #'append (mapcar f m)))

(defmethod monad-return ((monad (eql :list)) x)  (list x))
(defmethod check-monad-return-type ((monad (eql :list)) (value list)) value)


;; It seems I must provide fmap for list
(defmethod fmap (f (m list))
  (mapcar f m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

