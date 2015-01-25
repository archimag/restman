;; webapp.lisp

(asdf:operate 'asdf:load-op '#:cl-who)
(asdf:operate 'asdf:load-op  '#:restas.hunchentoot)


(restas:define-module #:restman.example.webapp
  (:use #:cl #:alexandria)
  (:export #:*default-message*))

(in-package #:restman.example.webapp)

(defparameter *default-message* "Hello world")

(restas:define-route main ("" :method '(:get :post))
  (let ((msg (or (restas:parameter "message") *default-message*)))
    #|------------------------------------------------------------------------|#
    (restas:set-cookie "message" :value (restas:parameter "message"))
    #|------------------------------------------------------------------------|#
    (let ((fileinfo (restas:post-parameter "file")))
      (who:with-html-output-to-string (out)
        (:html
         (:body
          (:h1 (who:str (format nil "~A (~A)" msg (restas:cookie-in "message"))))
          ((:form :method :post :enctype "multipart/form-data")
           (:p
            ((:input :name "message")))
           (:p
            ((:input :name "file" :type "file")))
           ((:input :type "submit" :value "Change title")))
          (:pre (who:str (if fileinfo
                           (read-file-into-string (first fileinfo)))))
              ))))))

