#|
  This file is a part of trivialib.typevar project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivialib.typevar
  (:use :cl :alexandria :trivia :trivia.fail)
  (:export
   #:polymorphic-object
   #:polymorphic-class
   #:polymorphic-class-hash
   #:polymorphic-class-typevars
   #:ensure-polymorphic-class-instance
   #:type-unify
   #:make-constructor-form
   #:define-constructor
   #:default-initform
   #:type-unify1
   #:type-unification-error))
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

(defmethod make-instance ((class polymorphic-class)
                          &key typevals initforms &allow-other-keys)
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
       (format t "~&; Instantiating polymorphic class ~a" //-name)
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

(defmethod c2mop:finalize-inheritance :after ((class polymorphic-class))
  (eval (make-constructor-form class)))

(defmacro define-constructor (polymorphic-class)
  (make-constructor-form (find-class polymorphic-class)))

(defun make-constructor-form (class)
  (let* ((slotds (c2mop:class-slots class))
         (names (mapcar #'c2mop:slot-definition-name slotds))
         (types (mapcar #'c2mop:slot-definition-type slotds)))
    `(defun ,(class-name class) (&optional ,@names)
       (make-instance (make-instance ',(class-name class) 
                        :typevals (mapcar #'cdr
                                          (type-unify
                                           ',(polymorphic-class-typevars class)
                                           ',types
                                           (list ,@(mapcar (lambda (x) `(wrap-fixnum (type-of ,x))) names))))
                        :initforms (list ,@(mapcar (lambda (x) `(default-initform ,x)) names)))
                     ,@(alist-plist
                        (mapcar (lambda (name)
                                  (cons (make-keyword name) name))
                                names))))))


;;; type unification
;; 1. Any type variable unifies with any type expression, and is
;; instantiated to that expression. A specific theory might restrict this
;; rule with an occurs check.
;; 
;; 2. Two type constants unify only if they are the same type.
;; 
;; 3. Two type constructions unify only if they are applications of the
;; same type constructor and all of their component types
;; recursively unify.

(define-condition type-unification-error (program-error) ())

(defun type-unify (typevars templates types)
  (ematch* (templates types)
    ((nil nil)
     nil)
    (((list* tmpl tmpl*) (list* type type*))
     (multiple-value-match (type-unify1 typevars tmpl type)
       ((nil nil)
        (error 'type-unification-error))
       ((results t)
        (labels ((rec (results typevars tmpl*)
                   (ematch results
                     (nil (values typevars tmpl*))
                     ((list* (cons var val) results)
                      (rec results
                           (remove var typevars)
                           (subst val var tmpl*))))))
          (multiple-value-match (rec results typevars tmpl*)
            ((typevars tmpl*)
             (append results
                     (type-unify typevars tmpl* type*))))))))))

(defun type-unify1 (typevars template type)
  (if (atom type)
      (type-unify1-atomic typevars template type)
      (type-unify1-compound typevars template type)))

(defun type-unify1-atomic (typevars template type)
  (ematch template
    ((and (symbol)
          (guard typevar (member typevar typevars)))
     ;; template is an atomic typevar
     (values (list (cons typevar type)) t))
    ('* (warn "Avoid using * in type specification, it matches everything!")
        (values nil t))
    ((symbol)
     ;; template is a standard atomic type
     (if (subtypep type template)
         (values nil t)
         nil))
    ((integer)
     ;; FIXME possibly a dimension specifier of array
     (ematch type
       ((eq template) (values nil t))
       ('* (values nil t))
       (_ nil)))
    ;; compound types
    ((list* 'or rest)
     (dolist (tmp rest)
       (multiple-value-match
           (type-unify1 typevars tmp type)
         ((mapping t) (return-from type-unify1-atomic (values mapping t))))))
    ((list* (and typespec (or 'and 'eql 'member)) _)
     (error "~a is not supported in typevar!" typespec))))

(defun type-unify1-compound (typevars template type)
  (ematch type
    ((list* (or (integer) '*) _)
     ;; FIXME possibly a dimension specifier of array
     (ematch template
       ((and (symbol)
             (guard typevar (member typevar typevars)))
        ;; template is an atomic typevar
        (values (list (cons typevar type)) t))
       ('* (warn "Avoid using * in type specification, it matches everything!")
           (values nil t))
       ((list* _)
        (unless (= (length template) (length type))
          (fail))
        (values (mappend (lambda (e1 e2)
                           (multiple-value-match
                               (type-unify1 typevars e1 e2)
                             ((mapping t) mapping)
                             ((_ nil) (return-from type-unify1-compound nil))))
                         template type)
                t))))
    ((list* head elements1)
     (ematch template
       ((and (symbol)
             (guard typevar (member typevar typevars)))
        ;; template is an atomic typevar
        (values (list (cons typevar type)) t))
       ('* (warn "Avoid using * in type specification, it matches everything!")
           (values nil t))
       ((symbol)
        ;; template is a standard atomic type.
        ;; FIXME: string -> (array char *)
        nil)
       ((list* (eq head) elements2)
        (let ((max (max (length elements1) (length elements2))))
          (values (mappend (lambda (e1 e2)
                             (multiple-value-match
                                 (type-unify1 typevars e1 e2)
                               ((mapping t) mapping)
                               ((_ nil) (return-from type-unify1-compound nil))))
                           (pad max '* elements2)
                           (pad max '* elements1))
                  t)))))))

(defun pad (max thing list)
  (append list
          (make-list (- max (length list)) :initial-element thing)))

;;; default-initform

(defgeneric default-initform (x))
(defmethod default-initform ((x number))
  (coerce 0 (class-of x)))
(defmethod default-initform ((x ratio))
  ;; 1/1 is not a ratio. its devisor should be > 1.
  ;; however we stick to zero.
  0)
(defmethod default-initform ((x sequence))
  (coerce nil (class-of x)))
(defmethod default-initform ((x standard-object))
  (make-instance (type-of x)))
(defmethod default-initform ((x structure-object))
  (make-instance (type-of x)))


