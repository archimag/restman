;; request.lisp

(in-package #:restman.simulator)

(defclass simulator-listener ()
  ((port :initform 8080 :initarg :port :reader restas:listener-port)
   (address :initform nil :initarg :address :reader restas:listener-address)
   (ssl-p :initform nil :initarg :ssl :reader restas:listener-ssl-p)))

(defclass simulator-request ()
  ((get-parameters :initform nil :reader restas:request-get-parameters)
   (post-parameters :initform nil :initarg :post-parameters :reader restas:request-post-parameters)
   (cookies-in :initform nil :reader restas:request-cookies-in)
   (listener :reader restas:request-listener)
   #|-------------------------------------------------------------------------|#
   (uri :initarg :uri :reader restas:request-request-uri)
   (method :initarg :method :reader restas:request-request-method)
   (server-protocol :initarg :server-protocol :initform "http/1.1" :reader restas:request-server-protocol)
   (headers :initform nil :initarg :headers-in :reader restas:request-headers-in)
   (remote-addr :initform nil :initarg :remote-addr :reader restas:request-remote-address)
   (remote-port :initform nil :initarg :remote-port :reader restas:request-remote-port)
   (raw-post-data :initform nil :initarg :raw-post-data)
   #|-------------------------------------------------------------------------|#
   (state :initform nil :initarg :state :reader request-state)))

(defparameter *scanner-cookie-split*
  (cl-ppcre:create-scanner ";[ \\s\\t]+")
  "Scanner for splitting up cookies.")

(defmethod shared-initialize :after ((request simulator-request) slot-names &key &allow-other-keys)
  #|--------------------------------------------------------------------------|#
  (iter (for name-value in (split-sequence #\& (puri:uri-query (restas:request-request-uri request))))
        (destructuring-bind (name &optional value) (split-sequence #\= name-value)
          (push (cons (restas:url-decode name)
                      (restas:url-decode value))
                (slot-value request 'get-parameters))))
  #|--------------------------------------------------------------------------|#
  (setf (slot-value request 'listener)
        (let* ((uri (restas:request-request-uri request))
               (ssl-p (eql (puri:uri-scheme uri) :https)))
          (make-instance 'simulator-listener
                         :port (or (puri:uri-port uri)
                                   (if ssl-p 80 443))
                         :address nil
                         :ssl ssl-p)))
  #|--------------------------------------------------------------------------|#
  (setf (slot-value request 'headers)
        (iter (for (key . value) in (slot-value request 'headers))
              (collect
                  (cons (make-keyword (string-upcase key))
                        value))))
  #|--------------------------------------------------------------------------|#
  (let ((cookies (restas:header-in :cookie request)))
    (when cookies
      (setf (slot-value request 'cookies-in)
            (iter (for cookie in (ppcre:split *scanner-cookie-split* cookies))
                  (destructuring-bind (name value) (split-sequence #\= cookie)
                    (collect
                        (cons name value))))))))

(defmethod request-state (request)
  nil)

(defmethod restas:request-query-string ((request simulator-request))
  (puri:uri-query (restas:request-request-uri request)))

(defmethod restas:request-script-name ((request simulator-request))
  (puri:uri-path (restas:request-request-uri request)))

(defmethod restas:request-raw-post-data ((request simulator-request))
  (let ((data (slot-value request 'raw-post-data)))
    (if data
        (read-file-into-byte-vector (parse-native-namestring data))))) 

(defun hash-table-request (ht)
  (flet (($ (key)
           (gethash key ht)))
    #|------------------------------------------------------------------------|#
    (let ((uri (puri:parse-uri ($ "url"))))
      (make-instance 'simulator-request
                     :uri uri
                     :method (find-symbol ($ "method") '#:keyword)
                     :server-protocol ($ "serverProtocol")
                     :headers-in (hash-table-alist ($ "headers"))
                     :post-parameters (if ($ "data")
                                          (iter (for (key value) in-hashtable ($ "data"))
                                                (collect
                                                    (cons key
                                                          (if (listp value)
                                                              (cons (parse-native-namestring (car value))
                                                                    (cdr value))
                                                              value)))))
                     :state (if ($ "state") (hash-table-alist ($ "state")))))))

