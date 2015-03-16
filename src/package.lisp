#|
  This file is a part of trivialib.typevar project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivialib.typevar
  (:use :cl :alexandria :trivia)
  (:export
   #:polymorphic-object
   #:polymorphic-class
   #:polymorphic-class-hash
   #:polymorphic-class-typevars
   #:ensure-polymorphic-class-instance))
(in-package :trivialib.typevar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (polymorphic-object (:include structure-object)))

  (defclass polymorphic-class (standard-class)
       ((typevars :type list
                  :initarg :typevars
                  :reader polymorphic-class-typevars)
        (hash :type hash-table
              :initform (make-hash-table :test #'equal)
              :reader polymorphic-class-hash))))

(defmethod c2mop:validate-superclass
    ((class polymorphic-class) (super standard-class))
  t)

(defun specialized-structure-name (name typevals)
  (intern (format nil "~a~{/~a~}" name typevals)))

(defmethod make-instance :around ((class polymorphic-class)
                                  &key typevals &allow-other-keys)
  (ematch class
    ((polymorphic-class typevars hash)
     (assert (= (length typevars)
                (length typevals))
             nil "length differs between ~a and ~a" typevars typevals)
     (or (gethash typevals hash)
         (setf (gethash typevals hash)
               (call-next-method))))))

(defmethod make-instance ((class polymorphic-class)
                          &key typevals initforms &allow-other-keys)
  (ematch class
    ((polymorphic-class typevars)
     (let ((//-name (specialized-structure-name (class-name class) typevals))
           (real-types (mapcar (lambda-match
                                 ((c2mop:slot-definition type)
                                  (reduce (lambda (type pair)
                                            (match pair
                                              ((cons var val)
                                               (subst val var type))))
                                          (mapcar #'cons typevars typevals)
                                          :initial-value type)))
                               (c2mop:class-direct-slots class))))
       (prog1
         (make-instance 'structure-class
            :name //-name
            :direct-superclasses (list (find-class 'polymorphic-object))
            :direct-slots
            (mapcar (lambda-match* 
                      (((c2mop:slot-definition name) type initform)
                       (list :name name
                             :type type
                             :initargs (list (make-keyword name))
                             :initform initform
                             :initfunction nil)))
                    (c2mop:class-direct-slots class)
                    real-types initforms))
         (mapc (lambda-match*
                 (((c2mop:slot-definition name) type)
                  (let ((accessor-name (symbolicate //-name '- name)))
                    (proclaim `(ftype (function (,//-name) ,type) ,accessor-name))
                    (proclaim `(inline ,accessor-name))
                    (eval `(defun ,accessor-name (object)
                             (declare (type ,//-name object))
                             (declare (optimize speed))
                             (assert (typep object ',//-name) nil
                                     'type-error
                                     :expected-type ',//-name
                                     :datum object)
                             (slot-value object ',name)))
                    ;; (setf (fdefinition accessor-name)
                    ;;       (lambda (object) (slot-value object name)))
                    ;; (compile accessor-name)
                    )))
               (c2mop:class-direct-slots class)
               real-types))))))

(defun ensure-polymorphic-class-instance (class typevals initforms)
  (make-instance class :typevals typevals :initforms initforms))

(define-compiler-macro ensure-polymorphic-class-instance (&whole whole class typevals initforms)
  (match* (class typevals initforms)
    (((class class) (list 'quote typevals) (list 'quote initforms))
     (make-instance class :typevals typevals :initforms initforms))
    (((list 'quote (and sym (symbol))) (list 'quote typevals) (list 'quote initforms))
     (make-instance (find-class sym) :typevals typevals :initforms initforms))
    ((_ _ _)
     whole)))




