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
        :key (compose #'string #'car)))

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
    (unless (emptyp (restas:request-raw-post-data request))
      (setf (gethash "rawPostData" obj)
            (restas:request-raw-post-data request)))
    #|------------------------------------------------------------------------|#
    (iter (for (name . value) in (sort-alist (restas:headers-in request)))
          (setf (gethash (http-word name) headers)
                value))
    #|------------------------------------------------------------------------|#
    obj))

(defun calc-checksum (bytes)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence  (ironclad:make-digest :sha1)
                              bytes)))

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
             (values "binary" (calc-checksum content))))
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

(defun native-namestring (pathname)
  #+sbcl (sb-ext:native-namestring pathname)
  #-sbcl (namestring pathname))

(defun parse-native-namestring (thing)
  #+sbcl (sb-ext:parse-native-namestring thing)
  #-sbcl (parse-namestring thing))

(defun re-format-json (obj stream)
  (labels ((wrap (o)
             (typecase o
               (hash-table
                (let ((ht (make-hash-table :test (hash-table-test o))))
                  (iter (for key in (sort (hash-table-keys o) #'string<))
                        (setf (gethash key ht)
                              (wrap (gethash key o))))
                  ht))
               (list (mapcar #'wrap o))
               (t o))))
    #|------------------------------------------------------------------------|#
    (yason:encode (wrap obj)
                  (yason:make-json-output-stream stream :indent t))))

(defun re-format-xml (obj stream)
  (labels ((sort-attrs (el)
             (let (attrs)
               #|-------------------------------------------------------------|#
               (iter (for attr in (xtree:all-attribute-nodes el))
                     (for name = (xtree:local-name attr))
                     (for ns = (xtree:namespace-uri attr))
                     (push (list name
                                 ns
                                 (xtree:text-content attr))
                           attrs)
                     (xtree:remove-attribute el name ns))
               #|-------------------------------------------------------------|#
               (iter (for (name ns value) in (sort attrs #'string< :key #'first))
                     (setf (xtree:attribute-value el name ns)
                           value))
               #|-------------------------------------------------------------|#
               (iter (for child in (xtree:all-childs el))
                     (when (xtree:element-p child)
                       (sort-attrs child))))))
    #|------------------------------------------------------------------------|#
    (sort-attrs obj)
    #|------------------------------------------------------------------------|#
    (xtree:serialize obj stream :pretty-print t)))
