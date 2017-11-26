(in-package #:asdf-user)

(asdf:defsystem #:cl-mersenne
  :serial t
  :description "Quick and dirty mersenne twister implementation"
  :depends-on ()
  :components ((:file "package")
               (:file "mersenne")))
