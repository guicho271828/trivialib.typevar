#|
  This file is a part of trivialib.typevar project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivialib.typevar
  (:use :cl :alexandria :trivia :trivia.fail :aspectm :trivialib.type-unify)
  (:export
   #:polymorphic-object
   #:polymorphic-class
   #:polymorphic-class-hash
   #:polymorphic-class-typevars
   #:make-constructor-form
   #:gtype))
(in-package :trivialib.typevar)

;;; class definition
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (polymorphic-object (:include structure-object)))

  (defclass polymorphic-class (standard-class)
       ((typevars :type list
                  :initarg :typevars
                  :reader polymorphic-class-typevars)
        (hash :type hash-table
              :initform (make-hash-table :test #'equal)
              :reader polymorphic-class-hash))))

(defmethod c2mop:validate-superclass ((class polymorphic-class) (super standard-class))
  t)

(defun specialized-structure-name (name typevals)
  (intern (format nil "~a~{/~a~}" name typevals)))

(defun wrap-fixnum (type)
  (match type
    ;; since SBCL broke this
    ((list 'integer 0 MOST-POSITIVE-FIXNUM)
     'fixnum)
    (type type)))

(defmethod make-instance :around ((class polymorphic-class)
                                  &key typevals &allow-other-keys)
  (let ((typevals (mapcar #'wrap-fixnum typevals)))
    (ematch class
      ((polymorphic-class typevars hash)
       (assert (= (length typevars)
                  (length typevals))
               nil "length differs between ~a and ~a" typevars typevals)
       (or (gethash typevals hash)
           (setf (gethash typevals hash)
                 (call-next-method)))))))

(defmethod make-instance ((class polymorphic-class) &key typevals initforms &allow-other-keys)
  (ematch class
    ((polymorphic-class typevars)
     (let ((//-name (specialized-structure-name (class-name class) typevals))
           (real-types (mapcar (lambda-ematch
                                 ((c2mop:slot-definition type)
                                  (reduce (lambda (type pair)
                                            (ematch pair
                                              ((cons var val)
                                               (subst val var type))))
                                          (mapcar #'cons typevars typevals)
                                          :initial-value type)))
                               (c2mop:class-direct-slots class))))
       (format t "~&; Instantiating a grounded standard structure ~a" //-name)
       (prog1
         (make-instance 'structure-class
            :name //-name
            :direct-superclasses (list (find-class 'polymorphic-object))
            :direct-slots
            (mapcar (lambda-ematch* 
                      (((c2mop:slot-definition name) type initform)
                       (list :name name
                             :type type
                             :initargs (list (make-keyword name))
                             :initform initform
                             :initfunction nil)))
                    (c2mop:class-direct-slots class)
                    real-types initforms))
         ;; accessors
         (mapc (lambda-ematch*
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

;; (defun ensure-polymorphic-class-instance (class typevals initforms)
;;   (make-instance class :typevals typevals :initforms initforms))
;; 
;; (define-compiler-macro ensure-polymorphic-class-instance (&whole whole class typevals initforms)
;;   (match* (class typevals initforms)
;;     (((class class) (list 'quote typevals) (list 'quote initforms))
;;      (make-instance class :typevals typevals :initforms initforms))
;;     (((list 'quote (and sym (symbol))) (list 'quote typevals) (list 'quote initforms))
;;      (make-instance (find-class sym) :typevals typevals :initforms initforms))
;;     ((_ _ _)
;;      whole)))

(defun find-direct-slots (slot/keyword c)
  (or (find slot/keyword (c2mop:class-direct-slots c)
            :key #'c2mop:slot-definition-name)
      (find slot/keyword (c2mop:class-direct-slots c)
            :key #'c2mop:slot-definition-name
            :test #'string=)
      (some (lambda (c)
              (find-direct-slots slot/keyword c))
            (c2mop:class-direct-superclasses c))))


(defmethod initialize-instance :after ((class polymorphic-class) &key &allow-other-keys)
  (eval (make-finalize-form class)))
(defmethod c2mop:finalize-inheritance :after ((class polymorphic-class))
  (eval (make-constructor-form class)))

(defun make-finalize-form (class)
  `(defun ,(class-name class) (&rest args)
     (c2mop:finalize-inheritance ,class)
     (apply #',(class-name class) args)))

(defun make-constructor-form (class)
  (let* ((name (class-name class))
         (slotds (c2mop:class-slots class))
         (names (mapcar #'c2mop:slot-definition-name slotds))
         (types (mapcar #'c2mop:slot-definition-type slotds))
         (typevars (polymorphic-class-typevars class)))
    `(progn
       (defun ,name (&optional ,@names)
         (make-instance (make-instance ',name
                           :typevals (mapcar #'cdr
                                             (type-unify
                                              ',typevars
                                              ',types
                                              (list ,@(mapcar (lambda (x) `(wrap-fixnum (type-of ,x))) names))))
                           :initforms (list ,@(mapcar (lambda (x) `(default-initform ,x)) names)))
            ,@(alist-plist
               (mapcar (lambda (name)
                         (cons (make-keyword name) name))
                       names))))
       (deftype ,(symbolicate name '/) ,typevars
         (class-name
          (make-instance ',name
             :typevals (list ,@typevars)
             :initforms (mapcar #'default-initform/type (list ,@typevars))))))))

;;; default-initform

(defun default-initform (x)
  (default-initform/type (type-of x)))

(defun default-initform/type (type)
  (cond
    ((subtypep type 'sequence) (coerce nil type))
    ((subtypep type 'ratio) 0)
    ((subtypep type 'number) (coerce 0 type))
    ((subtypep type 'standard-object) (make-instance type))
    ((subtypep type 'structure-object) (make-instance type))))
    
;;; gtype

(defstruct gtype-info
  types
  lambda-expression)

(lispn:define-namespace gtype-info gtype-info)

;; thanks, myrkraverk #lisp !
(defmacro gtype (fname &rest types)
  `(progn
     (define-standard-hook (defun ,(symbolicate 'gtype- fname) :before) (name args &body body)
       (when (eq name ',fname)
         (setf (symbol-gtype-info ',fname)
               (make-gtype-info
                :types ',types
                :lambda-expression `(lambda ,args ,@body))))
         nil)
     (define-standard-hook (defun ,(symbolicate 'gtype- fname) :after) (name args &body body)
       ;; 
       )))



