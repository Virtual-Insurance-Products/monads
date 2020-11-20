

(asdf:defsystem :monads
  :description "Monads in Common Lisp"
  :author "VIP"
  :serial t
  :depends-on ("vip-utils" "anaphors") ; for seq macro
  :components ((:file "package")
               (:file "monads")
               (:file "continuation")
               (:file "list")
               (:file "maybe")
               )
  )
