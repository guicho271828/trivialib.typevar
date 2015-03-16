#|
  This file is a part of trivialib.typevar project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :trivialib.typevar.test
  (:use :cl
        :trivialib.typevar
        :fiveam
        :alexandria :trivia))
(in-package :trivialib.typevar.test)



(def-suite :trivialib.typevar)
(in-suite :trivialib.typevar)

;; run test with (run! test-name) 
;;   test as you like ...

(test trivialib.typevar
  (finishes
    (defclass kons ()
         ((kar :type a)
          (kdr :type b))
      (:metaclass polymorphic-class)
      (:typevars a b)))
  (finishes
    (defparameter *kons/fixnum*
                  (make-instance 'kons
                     :typevals '(fixnum fixnum)
                     :initforms '(0 0))))
  (finishes
    (defparameter *instance* (make-instance *kons/fixnum*)))
  (finishes
    (defparameter *instance2* (make-instance *kons/fixnum* :kar 1))))
