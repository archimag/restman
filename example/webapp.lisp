;; webapp.lisp

(asdf:operate 'asdf:load-op '#:cl-who)
(asdf:operate 'asdf:load-op  '#:restas.hunchentoot)


(restas:define-module #:restman.example.webapp
  (:use #:cl))

(in-package #:restman.example.webapp)

(restas:define-route main ("" :method '(:get :post))
  (who:with-html-output-to-string (out)
    (:html
     (:body
      (:h1 (who:str (or (restas:post-parameter "message")
                        "Hello world")))
      ((:form :method :post)
       ((:input :name "message"))
       ((:input :type "submit" :value "Change title")))))))

