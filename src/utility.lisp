;; encode.lisp

(in-package #:restman.utility)

(defun try-unpromisify (promise)
  (if (and (promisep promise) (promise-finished-p promise))
      (values-list (blackbird-base::promise-values promise))
      promise))

(defun http-word (keyword)
  (format nil
          "~{~:(~A~)~^-~}"
          (split-sequence #\- (symbol-name keyword))))

(defun sort-alist (alist)
  (sort (copy-alist alist)
        #'string<
        :key (compose #'symbol-name #'car)))

(defun request-hash-table (request)
  (let ((obj (make-hash-table :test 'equal))
        (headers (make-hash-table :test 'equal)))
    #|------------------------------------------------------------------------|#
    (setf (gethash "url" obj)
          (format nil
                  "http://~A~A"
                  (restas:request-host request)
                  (restas:request-uri request)))
    #|------------------------------------------------------------------------|#
    (setf (gethash "method" obj)
          (symbol-name (restas:request-method request)))
    #|------------------------------------------------------------------------|#
    (setf (gethash "serverProtocol" obj)
          (symbol-name (restas:server-protocol request)))
    #|------------------------------------------------------------------------|#
    (when (restas:remote-address request)
      (setf (gethash "remoteAddress" obj) (restas:remote-address request)
            (gethash "remotePort" obj) (restas:remote-port request)))
    #|------------------------------------------------------------------------|#
    (setf (gethash "headers" obj)
          headers)
    #|------------------------------------------------------------------------|#
    (setf (gethash "data" obj)
          (alist-hash-table (sort-alist (restas:post-parameters request)) :test 'equal))
    #|------------------------------------------------------------------------|#
    (iter (for (name . value) in (sort-alist (restas:headers-in request)))
          (setf (gethash (http-word name) headers)
                value))
    #|------------------------------------------------------------------------|#
    obj))

(defun reply-hash-table (reply &optional content)
  (let ((r (make-hash-table :test 'equal))
        (headers (make-hash-table :test 'equal)))
    #|------------------------------------------------------------------------|#
    (setf (gethash "headers" r)
          headers)
    #|------------------------------------------------------------------------|#
    (iter (for (name . value) in (sort-alist (restas:headers-out reply)))
          (setf (gethash (http-word name) headers)
                value))
    #|------------------------------------------------------------------------|#
    (setf (gethash "code" r)
          (restas:return-code reply))
    #|------------------------------------------------------------------------|#
    (when content
      (multiple-value-bind (mode data)
          (typecase content
            (string
             (values "text" content))
            (pathname
             (values "pathname" content))
            (otherwise
             (values "binary" (base64:usb8-array-to-base64-string content))))
        #|--------------------------------------------------------------------|#
        (setf (gethash "contentMode" r) mode
              (gethash "content" r) data)))
    #|------------------------------------------------------------------------|#
    r))
    
(defun encode-to-file (obj path)
  (with-output-to-file (out path :if-exists :supersede :if-does-not-exist :create)
    (yason:encode obj (yason:make-json-output-stream out :indent t))))


(defun string-to-symbol (str)
  (destructuring-bind (package name) (split-sequence #\: str)
    (ensure-symbol (string-upcase name)
                   (string-upcase package))))
    
