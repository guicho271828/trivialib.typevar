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
    (defparameter *instance2* (make-instance *kons/fixnum* :kar 1)))

  (finishes
   ;; automatically instantiate kons/single-float/single-float
   ;; it has compiler-macro
   (defparameter *instance3* (kons 1.0 0.0))))

(test type-unify1
  (is (equal '((a . fixnum))
             (type-unify1 '(a)
                         'a
                         'fixnum)))
  
  (is (equal '((a . fixnum))
             (type-unify1 '(a)
                         '(or float a)
                         'fixnum)))
  
  (is (equal nil
             (type-unify1 '(a)
                         '(or float fixnum)
                         'fixnum)))

  (is-false
   (nth-value 1 (type-unify1 '(a) 'float 'fixnum)))

  #+nil
  (is (equal '((a . string))
             (type-unify1 '(a)
                         '(and simple-array a)
                         'simple-string)))

  
  (signals error
    (equal '((a . string))
           (type-unify1 '(a)
                       '(and simple-array a)
                       'simple-string)))
  
  (is (equal '((a . char))
             (type-unify1 '(a)
                         '(array a *)
                         '(array char (3)))))

  (is (equal '((a . char) (b . (1 *)))
             (type-unify1 '(a b)
                         '(array a b)
                         '(array char (1 *)))))

  (is (equal '((a . char) (b . 4))
             (type-unify1 '(a b)
                         '(array a (* b))
                         '(array char (3 4)))))
  
  (is-false
   (type-unify1 '(a)
               '(array a (* 2))
               '(array char (3 4)))))

(test type-unify
  (is (equal '((a . fixnum)) (type-unify '(a) '(a) '(fixnum))))
  (is (equal '((a . fixnum)) (type-unify '(a) '(a a) '(fixnum fixnum))))
  (signals type-unification-error
    (type-unify '(a) '(a a) '(fixnum float)))
  (signals type-unification-error
    (type-unify '(a) '(a b) '(fixnum float)))
  (is (equal '((a . fixnum) (b . float)) (type-unify '(a b) '(a b) '(fixnum float)))))


