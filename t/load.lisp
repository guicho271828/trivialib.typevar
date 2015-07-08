(defpackage :trivialib.typevar.test.load
  (:use :cl :trivialib.typevar))
(in-package :trivialib.typevar.test.load)

(gtype myfunc)
(defun myfunc () "aaa" (print :hi!))

