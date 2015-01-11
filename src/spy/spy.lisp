;; spy.lisp

(in-package #:restman.spy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spy-reply (restas:reply-proxy)
  ((saved-data :initform nil :accessor reply-saved-data)))

(defmethod restas:abort-request-handler ((reply spy-reply) result)
  #|--------------------------------------------------------------------------|#
  (setf (reply-saved-data reply)
        (reply-hash-table reply result))
  #|--------------------------------------------------------------------------|#
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spy ()
  ((request-list :initform nil :accessor spy-request-list)
   (active-p :initform nil :accessor spy-active-p)))

(defgeneric start-spy (spy)
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy))
    (unless (spy-active-p spy)
      (setf (spy-request-list spy) nil
            (spy-active-p spy) t))))

(defgeneric finish-spy (spy)
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy))
    (when (spy-active-p spy)
      (setf (spy-request-list spy)
            (nreverse (spy-request-list spy)))
      (setf (spy-active-p spy) nil))))

(defgeneric register-record (spy request reply content)
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy) request reply content)
    (when (spy-active-p spy)
      (let ((req (request-hash-table request))
            (res (or (reply-saved-data reply)
                     (reply-hash-table reply content))))
        #|--------------------------------------------------------------------|#
        (setf (gethash "reply" req)
              res)
        #|--------------------------------------------------------------------|#
        (push req (spy-request-list spy))))))

(defgeneric export-spy-journal (spy pathname)
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy) pathname)
    (let ((obj (make-hash-table :test 'equal))
          (requests (spy-request-list spy)))
      #|----------------------------------------------------------------------|#
      (setf (gethash "requests" obj)
            requests)
      #|----------------------------------------------------------------------|#
      (encode-to-file obj pathname)
      #|----------------------------------------------------------------------|#
      (values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; decorator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spy-route (restas:proxy-route)
  ((spy :initarg :spy)))

(defmethod restas:process-route :around ((route spy-route) bindings)
  (let ((restas:*reply* (make-instance 'spy-reply :origin restas:*reply*)))
    (try-unpromisify
     (tap (call-next-method)
          (lambda (&rest args)
            (register-record (slot-value route 'spy)
                             restas:*request*
                             restas:*reply*
                             (car args)))))))

(defun @spy (spy)
  (named-lambda make-spy-route (route)
    (make-instance 'spy-route :target route :spy spy)))
