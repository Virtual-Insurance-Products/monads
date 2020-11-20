
(in-package :monads)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CONTINUATION MONAD

;; ... tbc
(declaim (optimize (speed 2) (debug 1)))

(defmethod monad-return ((monad (eql :continuation)) x)
  #'(lambda (k)
      (funcall k x)))

(defmethod call/cc (f)
  #'(lambda (k)
      (funcall k (funcall f k))))

(defmethod current-continuation ()
  #'(lambda (k)
      (funcall k k)))

;; I don't know if I should make a specific type here. Can I subclass the function type in lisp?
(defmethod bind ((m function) f)
  ;; (funcall m f)
  #'(lambda (k)
      (funcall m
               #'(lambda (value)
                   (funcall (funcall f value)
                            k)))))

(defmethod check-monad-return-type ((monad (eql :continuation)) (value function)) value)


;; It does allow us to implement this though:-
;; Does this give us an easy way to do CPS conversion? I think it does. Not 100% sure though - I'd have to check
(defun mfact (n)
  (with-monad :continuation
    (if (< n 3)
        (unit n)
        (seq
          y <- (mfact (- n 1))
          (unit (* y n))))))

;; I'm not entirely sure that this one wouldn't be more sensible
;; (defun run-continuation (k) (funcall k #'identity))

;; NOTE - if the continuation is called multiple times this will only return the last answer
(defun run-continuation (k)
  (let ((answer nil))
    (funcall k (lambda (value)
                 (setf answer value)))
    answer))

;; (funcall (mfact 10) (lambda (x) (error "Hi")))
;; (run-continuation (mfact 10))

;; (funcall (with-monad :continuation
;;            x <- (unit 1)
;;            y <- (unit 2)
;;            cont <- (current-continuation)
;;            (unit cont))
;;          'identity)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

