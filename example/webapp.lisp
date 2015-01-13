;; webapp.lisp

(asdf:operate 'asdf:load-op '#:cl-who)
(asdf:operate 'asdf:load-op  '#:restas.hunchentoot)


(restas:define-module #:restman.example.webapp
  (:use #:cl))

(in-package #:restman.example.webapp)

(restas:define-route main ("")
  (who:with-html-output-to-string (out)
    (:html
     (:body
      (:h1 (who:str (or (restas:get-parameter "message")
                        "Hello world")))
      ((:form :method :get)
       ((:input :name "message"))
       ((:input :type "submit" :value "Change title")))))))

