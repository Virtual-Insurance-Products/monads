(in-package :monads)

(defgeneric bind (m f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Since I have the macroexpand-dammit module I *could* write a code walker which would try and do type inference. If it can't it would just give up and require type annotations
;; I could also put type annotations and have CL just believe them.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I think I'm starting to get the hang of this. There is, of course, one missing part - we can't properly implement return
;; SO, what would be a convenient workaround?
;; something like a macrolet? To effectively declare the return type?

;; I'm really wondering whether I should reimplement the with-monad macro to do what cl-monad-macros does - provide a macrolet for unit (return) and bind
;; maybe I should also rename return to unit - it's confusing and shadows the normal return otherwise. Probably a good idea (before I start using these for anything)

;; the good thing is that I can make the macrolet change without changing the way it's used. I think I still quite like my seq macro rather than explicit progn and let things, although those could be used too.
;; actually there is some advantage to the progn! and it does make things look more lispy. I could always try both and see which seems nicest to use. Choices choices.

;; I still wonder if I can do anything about things not returning monadic results.

;; ONE nice thing about using macrolet all the time is that we can arrange for, for example, (seq x <- (foo) (unit x)) to macro expand identically to (seq (foo) :== x x), which is efficient.

;; I COULD even provide the identity monad as a default for seq and then without explicit monad declaration I could use the monadic notation (like do).


;; OTOH, if the API will be the same I can postpone the decision as to whether to provide an optimised macrolet implementation of the monads until later. I could even just optimize out certain cases. The definition of each monad is very simple anyway

;; ALSO, I could avoid the need to do (seq - (function-whose-return-id-dont-need)) by ALWAYS putting in the bind. I could then avoid the problem of not being able to do normal lisp things by either switching to an identity monad, or just using progn (and maybe checking for macros or special forms before doing the bind transformation). I'd have to do that for things like if to work. Still, the situation I have at the moment is pretty handy. I'm just tempted to strip the non monadic stuff out of seq - it's gotten a bit complicated. It *is* useful though.


;; In simple terms, the function of this macro is to declare and enforce (at runtime sadly) the return type of a function as being a monad of the given type.
;; The things defining a monad as being of a certain type are somewhat disconnected at the moment, though nonetheless straightforward
;; I expect I could fairly trivially abstract them out somehow, but they really are very simple.
(defmacro with-monad (type &body body)
  `(macrolet ((unit (n)
                `(monad-return ,,type ,n))
              (zero ()
                `(monad-zero ,,type))
              ;; !!! I'm not sure if this is really supposed to be called lift
              (monadic (f &rest args)
                `(monadic-function ,,type ,f ,@args)))
     (check-monad-return-type ,type
                              ;; may as well put in seq automatically...
                              ,(cps-seq-transform body))))


;; general case - return type did NOT check
(defmethod check-monad-return-type ((monad t) value)
  (unless (eq monad (intern (symbol-name (type-of value)) :keyword))
    (error "Monad expression did not return a monad of type ~A but instead returned ~A" monad value))
  value)

;; NOTE - bind should always return a monad. I could check for this when implementing bind methods - probably a good idea. Most of the time we will fail anyway due to the implementation of bind.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GENERAL CASE...

;; Here I can provide a default definition for bind in terms of fmap and join. This means that we can just define those two instead of bind if we want. Sometimes it's easier.

;; general case
(defmethod bind (m f)
  (join (fmap f m)))

;; We can provide a guess at a default implementation of fmap so that we get fmap for free provided we have bind and unit but it's a bit hackey...
;; This works pretty well though for anything I have a monad for
(defmethod fmap (f m)
  ;; This is just a guess of course
  (unless (symbolp (type-of m))
    (error "Can't guess at a monad type for object of type ~A" (type-of m)))
  (let ((return-type (intern (symbol-name (type-of m)) :keyword)))
    (bind m (lambda (x)
              (monad-return return-type
                            (funcall f x))))))

;; We can't provide a default definition of join
;; note that if you don't define EITHER bind OR fmap and join then you don't have a functioning monad
(defmethod join (m)
  (error "No join defined for type ~A" (type-of m)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this kind of gives us and with binding. I hope it's efficient
(defmacro with-truth-monad (&body forms)
  `(flet ((truth-bind (m f)
            (if m (funcall f m))))
     (macrolet ((unit (n) n)
                (bind (m f)
                  `(truth-bind ,m ,f)))
       
       (seq ,@forms))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; IDENTITY MONAD

;; (this is somewhat pointless!)
;; I should actually probably wrap this

;; (defmethod bind (m f)
;;   (funcall f m))

;; (defmethod monad-return ((monad (eql :identity)) x) x)

;; (defmethod check-monad-return-type ((monad (eql :identity)) value) value)




;; which I already had a shorthand notation for anyway. By implementing this concept I've really just generalised this whole continuation rewrite


;; NOTE - I must be careful here because this allows anything to be treated as a monad. Only things which are declared as other specific types of monad would not, now, be handled monadically.
;; monadic binding for lisp lists will now ALWAYS use the lisp monad.



;; OTHER IDEAS

;; What other kinds of monad would be useful?
;; Maybe database access monads?
;; That would probably be a lot like the list monad perhaps. I really don't know what that would look like at the moment.

;; since the list monad gives us indeterminite results it should, somehow, be possible to write a stream monad to help operations on infinite streams of values. We would need somehow to get it to stop.
;; We would sort of need a generator - maybe a general generator monad? 

;; should I define a monadic zero - mzero - to use as a 'null' return. For :maybe that would be Nothing. For :list it would be nil

;; are monads of any use in writing GUIs? Would that be an easy way of doing it?

;; is the continuation monad something useful? Apparently it enables call/cc

;; threading monad? Would that be useful?




;; some other utilities...

;; This would be useful:-

;; *** This could be trivially rewritten in terms of seq without the explicit bind or lambda
;; I'm sure these two things could be generalised...
(defmacro if* (test true &optional false)
  (let ((condition (gensym)))
    `(bind ,test
           #'(lambda (,condition)
               (if ,condition
                   ,true
                   ,(or false '(unit nil)))))))

(defmacro aif* (test true &optional false)
  (let ((condition (gensym)))
    `(bind ,test
           #'(lambda (,condition)
               (aif ,condition
                    ,true
                    ,(or false '(unit nil)))))))

(defmacro mprogn (&body forms)
  `(seq
     ,@(loop for (f . rest) on forms
            when rest
          collect '-
          collect f)))

;; let's make an upside down mprogn: first-of
(defmacro mprogn* (first &body forms)
  (let ((value (gensym "value")))
    `(seq
       ,value <- ,first
       ,@(loop for x in forms
            collect '-
            collect x)
       (unit ,value))))


;; maybe some other monadic variants of control structures. The above means we can go (if (some-monadic-function) ...)
;; It's tempting to have seq or with-monad do a macrolet of if -> if*, BUT there might be cases where we do need to use the normal one. It might be avoidable though.
;; otherwise we sort of need to go (if* (unit (normal-function)) etc)
;; of course, we could provide other monadic variants of these things.
;; The important thing is to go through bind to allow the composition of the the test and body.

;; cond for example would be very useful




;; We will need monadic versions of certain useful utility thingies to avoid repetetive code...


;; this provides a general way of turning a function (f x y) -> z into a function (f Mx My) -> Mz returning some monad
;; the with-monad macro now provides a (lift ) macro to specialise this for the monad in question.
;; Obviously if you want this to deal with non monadic arguments just use (unit), although you could just put unit around the whole thing
;; !!! Is this lift? I'm not sure
(defmacro monadic-function (monad function &rest args)
  (let ((arg-names (loop for * in args collect (gensym))))
    `(seq
       ,@(loop for a in arg-names
            for arg in args
            collect a collect '<- collect arg)
       (monad-return ,monad (,function ,@arg-names))
       )))



(defun mmapcar (monad f &rest lists)
  (if (first lists)
      (monadic-function monad
                        cons
                        (apply f (mapcar #'first lists))
                        (apply #'mmapcar (cons monad (cons f (mapcar #'cdr lists)))))
      (monad-return monad nil)))


(defun indices (list)
  (loop for * in list for i from 0 collect i))


;; This adds an index parameter list
(defun mmapcar-i (monad f &rest lists)
  (apply #'mmapcar (cons monad
                         (cons f
                               (cons (indices (first lists))
                                     lists)))))

;; filter a list...
;; This is actually completely unnecessary because in the context of a monad we can just say (monadic filter (unit f) monadic-list)
;; I might also modify the monadic-function macro above so that it can handle monadic AND non monadic arguments. That shouldn't be a problem
;; that way we can avoid (unit) if we like (not that it's a problem really).
(defun mfilter (monad f list)
  (monadic-function monad
                    filter (monad-return monad f) list))

;; aren't the above definitions of mmapcar and mmapcar-i unnecessary because we can just a simple call to monadic to deal with them, or am I confused somehow?

(defmacro mcall-next-method? (&rest r)
  "Like call-next-method? but for monads - does (unit nil) if there is no next method."
  `(if (next-method-p)
       (call-next-method ,@r)
       (unit nil)))
