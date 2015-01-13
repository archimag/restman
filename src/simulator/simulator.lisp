;; request.lisp

(in-package #:restman.simulator)

(defclass simulator ()
  ((name :initform :name :initarg :name :reader simulator-name)
   (requests :initform :nil :initarg :requests :reader simulator-requests)
   (last-replies :reader simulator-last-replies)
   (run-results :initform nil :accessor simulator-run-results)))

(defmethod shared-initialize :after ((simulator simulator) slot-names &key pathname &allow-other-keys)
  #|--------------------------------------------------------------------------|#
  (setf (slot-value simulator 'last-replies)
        (make-hash-table :test 'equal))
  #|--------------------------------------------------------------------------|#
  (when pathname
    #|------------------------------------------------------------------------|#
    (setf (slot-value simulator 'name)
          (namestring pathname))
    #|------------------------------------------------------------------------|#
    (setf (slot-value simulator 'requests)
          (gethash "requests" (yason:parse pathname)))))

(defgeneric reformat-json-string (simulator json)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) (json string))
    (with-output-to-string(out)
      (yason:encode (yason:parse json)
                    (yason:make-json-output-stream out :indent t)))))

(defgeneric reformat-html-string (simulator html)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) (html string))
    (handler-bind ((warning #'muffle-warning))
      (html:with-parse-html (doc (format nil "<div>~A</div>" html))
        (xtree:serialize (xtree:first-child (xtree:first-child (xtree:root doc)))
                         :to-string
                         :pretty-print t)))))

(defgeneric format-string-diff (simulator a b)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) (a string) (b string))
    (with-output-to-string (stream)
      (diff:render-diff (diff:generate-seq-diff 'diff:unified-diff
                                                (split-sequence #\Newline a)
                                                (split-sequence #\Newline b))
                        stream))))

(defgeneric format-content-diff (simulator a b content-type)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) (a string) (b string) content-type)
    (cond
      ((starts-with-subseq "application/json" content-type)
       (format-string-diff simulator
                           (reformat-json-string simulator a)
                           (reformat-json-string simulator b)))
      ((starts-with-subseq "text/html" content-type)
       (format-string-diff simulator
                           (reformat-html-string simulator a)
                           (reformat-html-string simulator b)))
      (t
       (format-string-diff simulator a b)))))

(defgeneric compare-failed (simulator type &optional control-string &rest format-arguments)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) type &optional control-string &rest format-arguments)
    (list :status :error
          :type type
          :message (apply #'format nil control-string format-arguments))))

(defgeneric compare-success (simulator)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator))
    (list :status :success)))


(defun trivial-compare (simulator a b error-type)
  (if (not (equal a b))
      (compare-failed simulator error-type "~A instead of ~A" a b)))
  

(defgeneric compare-http-code (simulator origin-reply test-reply)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) reply1 reply2)
    (trivial-compare simulator
                     (gethash "code" reply1)
                     (gethash "code" reply2)
                     :code-diff)))

(defgeneric compare-content-type (simulator origin-reply test-reply)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) reply1 reply2)
    (trivial-compare simulator
                     (gethash "Content-Type" (gethash "headers" reply1))
                     (gethash "Content-Type" (gethash "headers" reply2))
                     :content-type-diff)))

(defgeneric compare-content-mode (simulator origin-reply test-reply)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) reply1 reply2)
    (trivial-compare simulator
                     (gethash "contentMode" reply1)
                     (gethash "contentMode" reply2)
                     :content-mode-diff)))

(defgeneric compare-text-content (simulator origin-reply test-reply)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) reply1 reply2)
    (if (string= (gethash "contentMode" reply1) "text")
        #|--------------------------------------------------------------------|#
        (let ((content1 (gethash "content" reply1))
              (content2 (gethash "content" reply2)))
          #|------------------------------------------------------------------|#
          (if (not (string= content1 content2))
              (compare-failed simulator
                              :content-diff
                              (format-content-diff simulator
                                                   content1
                                                   content2
                                                   (gethash "Content-Type"
                                                            (gethash "headers" reply1)))))))))

(defgeneric compare-binary-content (simulator origin-reply test-reply)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) reply1 reply2)
    (if (and (string= (gethash "contentMode" reply1) "binary")
             (not (equalp (gethash "content" reply1)
                          (gethash "content" reply2))))
        (compare-failed simulator
                        :content-diff
                        "Binary content is different"))))

(defgeneric compare-replies (simulator request origin-reply test-reply)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) request reply1 reply2)
    (list* :url (gethash "url" request)
           :method (gethash "method" request)
           (or (if (not reply1)
                   (compare-failed simulator
                                   :corrent-undefined
                                   "Correct REPLY is undefined"))
               (compare-http-code simulator reply1 reply2)
               (compare-content-type simulator reply1 reply2)
               (compare-content-mode simulator reply1 reply2)
               (compare-text-content simulator reply1 reply2)
               (compare-binary-content simulator reply1 reply2)
               (compare-success simulator)))))

(defgeneric print-run-results (simulator &optional stream)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) &optional (stream *standard-output*))
    #|------------------------------------------------------------------------|#
    (let ((cl-ansi-text:*enabled* (interactive-stream-p stream)))
      (cl-ansi-text:with-color (:black :stream stream :style :background)
        (cl-ansi-text:with-color (:white :stream stream)
          (format stream "=== RESULTS FOR ~A ===" (simulator-name simulator)))))
    (format stream "~&")
    #|------------------------------------------------------------------------|#
    (iter (for item in (simulator-run-results simulator))
          (cond
            #|----------------------------------------------------------------|#
            ((eql (getf item :status) :error)
             (let ((cl-ansi-text:*enabled* (interactive-stream-p stream)))
               (cl-ansi-text:with-color (:red :stream stream)
                 (format stream
                         "ERROR (~(~A~)): ~A ~A~&"
                         (getf item :type)
                         (getf item :method)
                         (getf item :url))))
             (when (getf item :message)
               (format stream "~A~&" (getf item :message))))
            #|----------------------------------------------------------------|#
            (t
             (let ((cl-ansi-text:*enabled* (interactive-stream-p stream)))
               (cl-ansi-text:with-color (:green :stream stream)
                 (format stream
                         "PASS: ~A ~A~&"
                         (getf item :method)
                         (getf item :url)))))))
    #|------------------------------------------------------------------------|#
    (let ((cl-ansi-text:*enabled* (interactive-stream-p stream)))
      (cl-ansi-text:with-color (:cyan :stream stream)
          (format stream
                  "Summary: ~A passed, ~A differ(s)~&"
                  (count :success (simulator-run-results simulator) :key (rcurry #'getf :status))
                  (count :error (simulator-run-results simulator) :key (rcurry #'getf :status)))))))

(defgeneric find-correct-reply (simulator request-id)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) request-id)
    (gethash "reply"
             (find request-id
                   (simulator-requests simulator)
                   :key (curry #'gethash "id")
                   :test #'string=))))

(defgeneric run (simulator obj &key stream environment)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) (module symbol) &rest args &key &allow-other-keys)
    (apply #'run simulator (make-route-map module) args))
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) (route-map routes:mapper) &key (stream *standard-output*) environment)
    (let ((requests (mapcar (rcurry 'apply-environment environment)
                            (simulator-requests simulator)))
          (last-replies (simulator-last-replies simulator)))
      #|----------------------------------------------------------------------|#
      (setf (simulator-run-results simulator)
            nil)
      #|----------------------------------------------------------------------|#
      (clrhash (simulator-last-replies simulator))
      #|----------------------------------------------------------------------|#
      (labels ((resolve ()
                 (setf (simulator-run-results simulator)
                       (nreverse (simulator-run-results simulator)))
                 (print-run-results simulator stream))
               #|-------------------------------------------------------------|#
               (reject (e)
                 (print e))
               #|-------------------------------------------------------------|#
               (impl ()
                 (let* ((request (pop requests))
                        (request-id (gethash "id" request)))
                   (chain (process-request route-map (hash-table-request request))
                     (:attach (reply)
                       #|-----------------------------------------------------|#
                       (setf (gethash request-id last-replies)
                             (reply-hash-table reply (reply-content reply)))
                       #|-----------------------------------------------------|#
                       (push (compare-replies simulator
                                              request
                                              (apply-environment (find-correct-reply simulator request-id)
                                                                 environment)
                                              (gethash request-id last-replies))
                             (simulator-run-results simulator))
                       #|-----------------------------------------------------|#
                       (if requests
                           (impl)
                           (resolve)))
                     (:catcher (e)
                       (reject e))))))
        (impl)
        (values)))))

(defgeneric fixate (simulator pathname)
  #|--------------------------------------------------------------------------|#
  (:method ((simulator simulator) (pathname pathname))
    #|------------------------------------------------------------------------|#
    (let ((last-replies (simulator-last-replies simulator)))
      (iter (for request in (simulator-requests simulator))
            (setf (gethash "reply" request)
                  (gethash (gethash "id" request) last-replies))))
    #|------------------------------------------------------------------------|#
    (let ((obj (make-hash-table :test 'equal)))
      (setf (gethash "requests" obj)
            (simulator-requests simulator))
      (encode-to-file obj pathname))
    #|------------------------------------------------------------------------|#
    (values)))
