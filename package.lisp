
(in-package :cl-user)

(defpackage :monads
  (:use :cl :vip-utils :anaphors)
  (:import-from #:vip-utils #:bind)
  (:export
   #:with-monad
   #:unit
   #:zero
   #:monadic
   #:monad-zero
   #:monad-return
   #:check-monad-return-type

   ;; I'm assuming these will need to be exported
   #:bind
   #:fmap
   #:join

   #:if*
   #:aif*
   #:mprogn
   #:mprogn*
   #:monadic-function
   #:mmapcar
   #:mmapcar-i
   #:mfilter

   #:mcall-next-method?
   
   ))
