;; dispatch.lisp

(in-package #:restman.simulator)

(defun make-route-map (module-name)
  (let ((module (make-instance 'restas::pkgmodule
                               :package (find-package module-name)
                               :context (restas:make-context)))
        (rmap (make-instance 'routes:mapper)))
    #|------------------------------------------------------------------------|#
    (restas:initialize-module-instance module (restas:module-context module))
    #|------------------------------------------------------------------------|#
    (restas::connect-module module rmap)
    #|------------------------------------------------------------------------|#
    rmap))

(defgeneric process-request (rmap request)
  #|==========================================================================|#
  (:method (rmap (request simulator-request))
    (let ((vars (mapcar (compose 'string-to-symbol #'car) (request-state request)))
          (vals (mapcar #'cdr (request-state request))))
      (progv vars vals
        (let ((bb:*promise-keep-specials* (append vars bb:*promise-keep-specials*)))
          (call-next-method)))))
  #|--------------------------------------------------------------------------|#
  (:method (rmap request)
    (with-promise (resolve reject)
      (let ((reply (make-instance 'simulator-reply)))
        #|----------------------------------------------------------------------|#
        (labels ((send-result (content)
                   (finish-reply reply content)
                   (resolve reply))
                 #|-------------------------------------------------------------|#
                 (send-file (path)
                   (setf (restas:content-type reply)
                         (or (restas:mime-type path)
                             (restas:content-type reply)
                             "application/octet-stream"))
                   (send-result (read-file-into-byte-vector path)))
                 #|-------------------------------------------------------------|#
                 (send-special-page (code)
                   (setf (restas:return-code reply)
                         code)
                   #|-----------------------------------------------------------|#
                   (setf (restas:content-type reply)
                         "text/html")
                   #|-----------------------------------------------------------|#
                   (send-result (restas:restas-status-message code))))
          #|--------------------------------------------------------------------|#
          (let ((restas:*request* request)
                (restas:*reply* reply))
            #|------------------------------------------------------------------|#
            (multiple-value-bind (route bindings)
                (routes:match rmap (restas:request-uri request))
              #|----------------------------------------------------------------|#
              (unless route
                (send-special-page restas:+http-not-found+))
              #|----------------------------------------------------------------|#
              (when route
                (chain (restas:process-route route bindings)
                  (:attach (result)
                           (cond
                             #|-----------------------------------------------------------|#
                             ((pathnamep result)
                              (send-file result))
                             #|-----------------------------------------------------------|#
                             (t
                              (send-result result))))
                  (:catcher (e)
                            (declare (ignore e))
                            (send-special-page restas:+http-internal-server-error+)))))))))))
