;; test-webapp.lisp

(asdf:operate 'asdf:load-op '#:restman)

(restas:define-module #:restman.example.webapp-test
  (:use #:cl #:alexandria #:iterate)
  (:export #:run #:recompare #:fixate))

(in-package #:restman.example.webapp-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load webapp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *example-directory*
  (merge-pathnames #P"example/"
                   (asdf:system-source-directory '#:restman)))

(load (merge-pathnames #P"webapp.lisp" *example-directory*))

(defparameter *requests-pathname*
  (merge-pathnames #P"data/webapp.json" *example-directory*))

(defparameter *replies-pathname*
  (merge-pathnames #P"data/data/" *example-directory*))

(defun reply-pathname (id basepath)
  (make-pathname :name id
                 :type "json"
                 :directory (pathname-directory basepath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simulator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass fionbio-simulator (restman.simulator:simulator)
  ((replies-source :initform nil :initarg :replies-source)))

(defmethod shared-initialize :after ((simulator fionbio-simulator) slot-names &key &allow-other-keys)
  (unless (slot-value simulator 'replies-source)
    (error "replies-source is required")))

(defmethod restman.simulator:find-correct-reply ((simulator fionbio-simulator) request-id)
  (let ((path (reply-pathname request-id
                              (slot-value simulator 'replies-source))))
    (if (fad:file-exists-p path)
        (yason:parse path))))

(defmethod restman.simulator:fixate ((simulator fionbio-simulator) pathname)
  #|--------------------------------------------------------------------------|#
  (ensure-directories-exist pathname)
  #|--------------------------------------------------------------------------|#
  (iter (for (id reply) in-hashtable (restman.simulator:simulator-last-replies simulator))
        (restman.utility:encode-to-file reply
                                        (reply-pathname id pathname)))
  #|--------------------------------------------------------------------------|#
  (values))

(defparameter *simulator*
  (make-instance 'fionbio-simulator
                 :requests-source *requests-pathname*
                 :replies-source *replies-pathname*))

(defun run (&optional env)
  (restman.simulator:run *simulator*
                         '#:restman.example.webapp
                         :environment (if env (restman.simulator:read-environment env))))

(defun recompare ()
  (restman.simulator:print-run-results *simulator*))

(defun fixate ()
  (restman.simulator:fixate *simulator* *replies-pathname*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass fionbio-spy (restman.spy:spy) ())

(defmethod restman.spy:generate-request-id ((spy fionbio-spy))
  (format nil "~6,'0d" (* (length (restman.spy:spy-requests-list spy)) 100)))

(defmethod restman.spy:export-spy-journal ((spy fionbio-spy) pathname &key replies-pathname)
  #|--------------------------------------------------------------------------|#
  (when replies-pathname
    (ensure-directories-exist replies-pathname))
  #|--------------------------------------------------------------------------|#
  (let ((requests-ht (make-hash-table :test 'equal))
        requests)
    #|------------------------------------------------------------------------|#
    (iter (for req in (restman.spy:spy-requests-list spy))
          (let ((request (copy-hash-table req))
                (reply (gethash "reply" req)))
            #|----------------------------------------------------------------|#
            (when replies-pathname
              (restman.utility:encode-to-file reply
                                              (reply-pathname (gethash "id" request)
                                                              replies-pathname)))
            #|----------------------------------------------------------------|#
            (remhash "reply" request)
            #|----------------------------------------------------------------|#
            (push request requests)))
    #|------------------------------------------------------------------------|#
    (setf (gethash "requests" requests-ht)
          (nreverse requests))
    #|------------------------------------------------------------------------|#
    (restman.utility:encode-to-file requests-ht pathname)))

(defparameter *spy* (make-instance 'fionbio-spy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route ~spy ("~/")
  (who:with-html-output-to-string (out)
    (:html
     (:body
      (:ul
       (:li
        (:a :href (who:str (restas:genurl '~spy/start))
            :target "_blank"
            "Start spy"))
       (:li
        (:a :href (who:str (restas:genurl '~spy/finish))
            "Finish spy")))))))

(restas:define-route ~spy/start ("~/start")
  (restman.spy:start-spy *spy*)
  (restas:redirect '-webapp-.main))

(restas:define-route ~spy/finish ("~/finish")
  (restman.spy:finish-spy *spy*)
  (restman.spy:export-spy-journal *spy*
                                  *requests-pathname*
                                  :replies-pathname *replies-pathname*)
  (restas:redirect '~spy))

(restas:mount-module -webapp- (#:restman.example.webapp)
  (:decorators (restman.spy:@spy *spy*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start web server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas.hunchentoot:start '#:restman.example.webapp-test :port 8080)

