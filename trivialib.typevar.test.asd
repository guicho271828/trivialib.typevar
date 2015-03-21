#|
  This file is a part of trivialib.typevar project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage trivialib.typevar.test-asd
  (:use :cl :asdf))
(in-package :trivialib.typevar.test-asd)


(defsystem trivialib.typevar.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:trivialib.typevar
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) (eval (read-from-string "(5am:run! :trivialib.typevar)"))))
