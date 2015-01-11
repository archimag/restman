;; reply.lisp

(in-package #:restman.simulator)

(defclass simulator-reply ()
  ((external-format :initform :utf-8 :accessor restas:reply-external-format)   
   (return-code :initform restas:+http-ok+ :accessor restas:reply-return-code)
   (headers-out :initform nil :accessor restas:reply-headers-out)
   (cookies-out :initform nil :accessor restas:reply-cookies-out)
   (content :initform nil :accessor reply-content)
   (alredy-send-p :initform nil :accessor alredy-send-p)))

(defmethod restas:reply-header-out (name (reply simulator-reply))
  (cdr (assoc name (restas:headers-out reply))))

(defmethod (setf restas:reply-header-out) (new-value name (reply simulator-reply))
  (let ((entry (assoc name (restas:headers-out reply))))
    (if entry
        (setf (cdr entry) new-value)
        (setf (slot-value reply 'headers-out)
              (acons name new-value (restas:headers-out reply))))
    new-value))

(defmethod restas:abort-request-handler ((reply simulator-reply) result)
  (finish-reply reply result))

(defun finish-reply (reply content)
  (unless (alredy-send-p reply)
    #|------------------------------------------------------------------------|#
    (setf (reply-content reply)
          content)
    #|------------------------------------------------------------------------|#
    (setf (alredy-send-p reply)
          t)
    #|------------------------------------------------------------------------|#
    (values)))

(defmacro define-reply-writer-wrapper (accessor reply-class)
  `(defmethod (setf ,accessor) :around (new-value (reply ,reply-class))
     (unless (alredy-send-p reply)
       (call-next-method))))

(define-reply-writer-wrapper restas:reply-external-format simulator-reply)
(define-reply-writer-wrapper restas:reply-return-code simulator-reply)
(define-reply-writer-wrapper restas:reply-headers-out simulator-reply)
(define-reply-writer-wrapper restas:reply-cookies-out simulator-reply)

