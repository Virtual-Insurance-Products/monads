
(in-package :monads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MAYBE MONAD...

;; there's a little bit of jiggering around here to implicitly declare the container type 'maybe' which I'm represnting with the keyword :maybe

;; These two, combined, form a Haskel like container type. Maybe I should simplify this into one struct with an optional slot? I would need a 'nothing' flag too. Well - this is pretty simple though.
;; The only thing is that if I did that the type checking would just become simpler.
(defstruct just x)
(defstruct nothing)

;; these two are just shorthand notations
(defun nothing () (make-nothing))
(defun just (x) (make-just :x x))


(defmethod bind ((m nothing) f)
  (declare (ignore f))
  m)

;; This check will usually be redundant - as long as the overall seq was enclosed in a (with-monad :maybe)
#+nil(defmethod bind ((m just) f)
  (check-monad-return-type :maybe
                           (funcall f (just-x m))))

(defmethod bind ((m just) f)
  (funcall f (just-x m)))

;; the return value needs to be explicitly typed somewhere. There's a macro to put it at the top in a block, or this could be used directly
;; this is because we can't dispatch on return type
(defmethod monad-return ((monad (eql :maybe)) x)
  (just x))


;; this should be a little bit more efficient because we aren't doing the dynamic method dispatch on (essentially) every monad
(defmacro with-maybe-monad (&body forms)
  `(flet ((maybe-bind (m f)
            (if (just-p m)
                (funcall f (just-x m))
                (nothing))))
     (macrolet ((unit (n)
                  `(just ,n))
                (bind (m f)
                  `(maybe-bind ,m ,f)))
       (seq ,@forms))))

;; runtime type checking...
(defmethod check-monad-return-type ((monad (eql :maybe)) (value nothing)) value)
(defmethod check-monad-return-type ((monad (eql :maybe)) (value just))    value)


;; EXAMPLE...

;; safe division. This makes sense - there is no answer to n/0
(defun safe-div (x y)
  (with-monad :maybe
    (if (= y 0)
        (nothing)
        (unit (/ x y)))))

;; (safe-div 1 0)
;; (safe-div 4 2)

(defun broken-safe-div (x y)
  (with-monad :maybe
    (if (= y 0)
        :broken
        (unit (/ x y)))))

;; (broken-safe-div 1 0)
