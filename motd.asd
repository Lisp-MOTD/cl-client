(require :asdf)

(asdf:defsystem #:motd
  :description "Lisp Message of the Day (Common Lisp Client)"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140817"
  :license "unlicense"
  :depends-on (#-quicklisp :drakma
               :cl-algebraic-data-type
               :track-best)
  :components ((:static-file "README.md")
               (:static-file "UNLICENSE")
               (:file "package")

               (:module "fetch"
                :depends-on ("package")
                :components ((:file "fetch")
                             #-quicklisp (:file "drakma"
                                                :depends-on ("fetch"))
                             #+quicklisp (:file "ql-http"
                                                :depends-on ("fetch"))))

               (:module "impl"
                :depends-on ("package")
                :components (#+ccl (:file "ccl")))

               (:file "helpers" :depends-on ("package"
                                             "fetch"
                                             "impl"))
               (:file "client" :depends-on ("package"
                                            "helpers"))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system '#:motd))))
  (asdf:load-system '#:motd-test)
  (funcall (find-symbol "RUN-ALL-TESTS" '#:motd-test)))

(asdf:defsystem #:motd-test
  :description "Unit tests for Lisp Message of the Day (Common Lisp Client)"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140819"
  :license "unlicense"
  :depends-on (#:nst #:motd #:cl-algebraic-data-type #:anaphora)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "criterion" :depends-on ("package"))
                             (:file "exports" :depends-on ("package"
                                                           "criterion"))
                             (:file "helpers" :depends-on ("package"))
                             (:file "run-all" :depends-on ("package"))))))
