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

(test trivialib.typevar::remove-larger
  (is (equal '(1) (trivialib.typevar::remove-larger '(1 5 2 3 4) #'<)))

  (is (set-equal '(1 "aa")
                 (trivialib.typevar::remove-larger
                  '(2 5 "aa" 1 "c" 3 4)
                  (lambda (a b)
                    (format t "~%~A ~A" a b)
                    (ematch* (a b)
                      (((integer) (integer))
                       (print (< a b)))
                      (((type string) (type string))
                       (print (string< a b)))
                      (_ (print :indifferent)
                         (values nil t)))))))

  (is (set-equal '(5 "c")
                 (trivialib.typevar::remove-smaller
                  '(2 5 "aa" 1 "c" 3 4)
                  (lambda (a b)
                    (format t "~%~A ~A" a b)
                    (ematch* (a b)
                      (((integer) (integer))
                       (print (< a b)))
                      (((type string) (type string))
                       (print (string< a b)))
                      (_ (print :indifferent)
                         (values nil t))))))))

(test trivialib.typevar::merge-mappings-as-or
  (is (equal '((a . fixnum) (b . fixnum))
             (trivialib.typevar::merge-mappings-as-or
              '((a . fixnum)) '((b . fixnum)))))

  (is (equal '((a . fixnum))
             (trivialib.typevar::merge-mappings-as-or
              '((a . fixnum)) '((a . fixnum)))))
  
  (is (equal '((a . integer))
             (trivialib.typevar::merge-mappings-as-or
              '((a . fixnum)) '((a . integer)))))
  (is (equal '((a . (or fixnum character)))
             (trivialib.typevar::merge-mappings-as-or
              '((a . fixnum)) '((a . character))))))

(test trivialib.typevar::merge-mappings-as-and
  (is (equal '((a . fixnum) (b . fixnum))
             (trivialib.typevar::merge-mappings-as-and
              '((a . fixnum)) '((b . fixnum)))))

  (is (equal '((a . fixnum))
             (trivialib.typevar::merge-mappings-as-and
              '((a . fixnum)) '((a . fixnum)))))
  
  (is (equal '((a . fixnum))
             (trivialib.typevar::merge-mappings-as-and
              '((a . fixnum)) '((a . integer)))))
  (is-false (trivialib.typevar::merge-mappings-as-and
             '((a . fixnum)) '((a . character)))))

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

  

  (is (equal '((a . simple-string))
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


(test kons
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
   (defparameter *instance3* (kons 1.0 0.0)))
  (kons 2 3)
  (kons 2 5.0))

(test kons2
  (finishes
    (defclass kons2 ()
         ((kar :type a)
          (kdr :type a))
      (:metaclass polymorphic-class)
      (:typevars a)))

  (finishes
    (kons2 0 0))

  
  (signals type-unification-error
    (kons2 0 0.0)))

(test deftype
  (finishes
    (proclaim '(ftype (function ((kons/ fixnum fixnum) (kons/ fixnum fixnum)) (kons/ fixnum fixnum)) add-kons)))
  (finishes
    (defun add-kons (kons1 kons2)
      (match* (kons1 kons2)
        (((kons/fixnum/fixnum :kar a :kdr b)
          (kons/fixnum/fixnum :kar c :kdr d))
         (kons (+ a c) (+ b d))))))

  (is-true (typep (add-kons (kons 3 5) (kons 5 10)) 'kons/fixnum/fixnum)))


(test ftype-with-typevar
  (finishes
    (gtype kdr-if a (kons/ a b) b))
  (finishes
    (defun kdr-if (a kons)
      (match* (kons1 kons2)
        (((kons/fixnum/fixnum :kar a :kdr b)
          (kons/fixnum/fixnum :kar c :kdr d))
         (kons (+ a c) (+ b d))))))

  (is-true (typep (add-kons (kons 3 5) (kons 5 10)) 'kons/fixnum/fixnum)))

(test gtype
  (finishes
    (gtype kdr-if a (kons/ a b) b)
    (defun kdr-if (x kons)
      (ematch kons
        ((kons (kar (eq x)) (kdr y))
         y)))

    ;; 

    ))

