;; test-webapp.lisp

(defparameter *example-directory*
  (merge-pathnames #P"example/"
                   (asdf:system-source-directory '#:restman)))

(defparameter *test-pathname*
  (merge-pathnames #P"webapp.json" *example-directory*))


(load (merge-pathnames #P"webapp.lisp" *example-directory*))

(asdf:operate 'asdf:load-op '#:restman)

(restas:define-module #:restman.example.webapp-test
  (:use #:cl)
  (:export #:run #:recompare #:fixate))

(in-package #:restman.example.webapp-test)

(defparameter *spy* (make-instance 'restman.spy:spy :genid-style :order))

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
  (restman.spy:export-spy-journal *spy* *test-pathname*)
  (restas:redirect '~spy))

(restas:mount-module -webapp- (#:restman.example.webapp)
  (:decorators (restman.spy:@spy *spy*)))

(restas.hunchentoot:start '#:restman.example.webapp-test :port 8080)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *simulator*
  (make-instance 'restman.simulator:simulator
                 :pathname *test-pathname*))

(defun run (&optional env)
  (restman.simulator:run *simulator*
                         '#:restman.example.webapp
                         :environment (if env (restman.simulator:read-environment env))))

(defun recompare ()
  (restman.simulator:print-run-results *simulator*))

(defun fixate ()
  (restman.simulator:fixate *simulator* *test-pathname*))
