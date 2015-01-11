;; restman.asd

(defsystem #:restman
    :depends-on (#:restas.core
                 #:blackbird #:local-time
                 #:yason #:diff #:cl-libxml2
                 #:cl-ansi-text)
    :serial t
    :pathname "src"
    :components ((:file "packages")
                 (:file "utility")
                 (:module "simulator"
                          :components ((:file "request")
                                       (:file "reply")
                                       (:file "dispatch")
                                       (:file "simulator")))
                 (:module "spy"
                          :components ((:file "spy")))))
