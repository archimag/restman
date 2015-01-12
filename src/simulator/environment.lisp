;; environment.lisp

(in-package #:restman.simulator)


(defgeneric read-environment (source)
  #|--------------------------------------------------------------------------|#
  (:method ((source pathname))
    (yason:parse source)))

(defgeneric apply-environment (obj env)
  #|--------------------------------------------------------------------------|#
  (:method (obj env)
    obj))

(defmethod apply-environment (obj (env pathname))
  (apply-environment obj (read-environment env)))

(defmethod apply-environment ((obj hash-table) (env hash-table))
  (let ((nobj (make-hash-table :test 'equal)))
    (iter (for (key value) in-hashtable obj)
          (setf (gethash key nobj)
                (apply-environment value env)))
    nobj))

(defmethod apply-environment ((obj string) (env hash-table))
  (flet ((replace-by-env (target start end m-start m-end &rest args)
           (declare (ignore start end args))
           (or (gethash (subseq target (1+ m-start) (1- m-end)) env)
               (subseq target m-start m-end))))
    #|----------------------------------------------------------------------|#
    (ppcre:regex-replace-all "{(\\w|-)+}" obj #'replace-by-env)))


     
                    
                             
    
