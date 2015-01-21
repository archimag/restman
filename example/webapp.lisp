;; webapp.lisp

(asdf:operate 'asdf:load-op '#:cl-who)
(asdf:operate 'asdf:load-op  '#:restas.hunchentoot)


(restas:define-module #:restman.example.webapp
  (:use #:cl)
  (:export #:*default-message*))

(in-package #:restman.example.webapp)

(defparameter *default-message* "Hello world")

(restas:define-route main ("" :method '(:get :post))
  (let ((msg (or (restas:parameter "message")
                 *default-message*)))
    #|------------------------------------------------------------------------|#
    (restas:set-cookie "message" :value (restas:parameter "message"))
    #|------------------------------------------------------------------------|#
    (who:with-html-output-to-string (out)
      (:html
       (:body
        (:h1 (who:str (format nil "~A (~A)" msg (restas:cookie-in "message"))))
        ((:form :method :post)
         ((:input :name "message"))
         ((:input :type "submit" :value "Change title"))))))))

