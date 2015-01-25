;; spy.lisp

(in-package #:restman.spy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spy-reply (restas:reply-proxy)
  ((saved-data :initform nil :accessor reply-saved-data)))

(defmethod restas:abort-request-handler ((reply spy-reply) result)
  #|--------------------------------------------------------------------------|#
  (setf (reply-saved-data reply)
        (reply-hash-table reply result))
  #|--------------------------------------------------------------------------|#
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spy ()
  ((genid-style :initform :uuid4 :initarg :genid-style  :reader spy-genid-style)
   (requests :initform nil :accessor spy-requests-list)
   (active-p :initform nil :accessor spy-active-p)
   (post-file-directory :initform #P"/tmp/" :initarg :file-directory :reader post-file-directory)))

(defgeneric start-spy (spy)
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy))
    (unless (spy-active-p spy)
      (setf (spy-requests-list spy) nil
            (spy-active-p spy) t))))

(defgeneric finish-spy (spy)
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy))
    (when (spy-active-p spy)
      (setf (spy-requests-list spy)
            (nreverse (spy-requests-list spy)))
      (setf (spy-active-p spy) nil))))

(defgeneric generate-request-id (spy)
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy))
    (case (spy-genid-style spy)
      (:uuid1
       (format nil "~A" (uuid:make-v1-uuid)))
      (:uuid4
       (format nil "~A" (uuid:make-v4-uuid)))
      (:order
       (format nil "~6,'0d" (* (length (spy-requests-list spy)) 100))))))


(defgeneric make-post-file-name (spy id)
  #|==========================================================================|#
  (:method ((spy spy) id)
    (make-pathname :name id
                   :directory (pathname-directory (post-file-directory spy)))))


(defgeneric register-record (spy request reply content)
  #|--------------------------------------------------------------------------|#
  (:method :around (spy request reply content)
    (when (spy-active-p spy)
      (call-next-method)))
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy) request (reply spy-reply)  content)
    (register-record spy
                     request
                     (or (reply-saved-data reply)
                         (reply-hash-table reply content))
                     content))
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy) request (reply hash-table) content)
    (register-record spy
                     (request-hash-table request)
                     reply
                     content))
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy) (request hash-table) (reply hash-table) content)
    (let ((id (generate-request-id spy)))
      #|----------------------------------------------------------------------|#
      (setf (gethash "id" request)
            id)
      #|----------------------------------------------------------------------|#
      (when (gethash "data" request)
        (iter (for (key value) in-hashtable (gethash "data" request))
              (when (listp value)
                (let ((s-path (make-post-file-name spy id)))
                  (destructuring-bind (path file-name content-type) value
                    (fad:copy-file path
                                   s-path
                                   :overwrite t)
                    (setf (gethash key (gethash "data" request))
                          (list (namestring s-path) file-name content-type)))))))
      #|----------------------------------------------------------------------|#
      (setf (gethash "reply" request)
            reply)
      #|----------------------------------------------------------------------|#
      (push request (spy-requests-list spy)))))

(defgeneric export-spy-journal (spy pathname &key replies-pathname)
  #|--------------------------------------------------------------------------|#
  (:method ((spy spy) pathname &key replies-pathname)
    (let ((obj (make-hash-table :test 'equal))
          (requests (spy-requests-list spy)))
      #|----------------------------------------------------------------------|#
      (when replies-pathname
        (let ((replies (make-hash-table :test 'equal)))
          #|------------------------------------------------------------------|#
          (setf requests
                (iter (for req in requests)
                      (let ((request (copy-hash-table req))
                            (reply-request (make-hash-table :test 'equal)))
                        #|----------------------------------------------------|#
                        (setf (gethash "id" reply-request)
                              (gethash "id" request))
                        #|----------------------------------------------------|#
                        (setf (gethash "reply" reply-request)
                              (gethash "reply" request))
                        #|----------------------------------------------------|#
                        (push reply-request
                              (gethash "requests" replies))
                        #|----------------------------------------------------|#
                        (remhash "reply" request)
                        #|----------------------------------------------------|#
                        (collect request))))
          #|------------------------------------------------------------------|#
          (encode-to-file replies replies-pathname)))
      #|----------------------------------------------------------------------|#
      (setf (gethash "requests" obj)
            requests)
      #|----------------------------------------------------------------------|#
      (encode-to-file obj pathname)
      #|----------------------------------------------------------------------|#
      (values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; decorator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spy-route (restas:proxy-route)
  ((spy :initarg :spy)))

(defmethod restas:process-route :around ((route spy-route) bindings)
  (let ((restas:*reply* (make-instance 'spy-reply :origin restas:*reply*)))
    (try-unpromisify
     (tap (call-next-method)
          (lambda (&rest args)
            (handler-case              
              (register-record (slot-value route 'spy)
                                 restas:*request*
                                 restas:*reply*
                                 (car args))
              (t (err)
                (break "A unforeseen error ~A" err)))
            )))))

(defun @spy (spy)
  (named-lambda make-spy-route (route)
    (make-instance 'spy-route :target route :spy spy)))
