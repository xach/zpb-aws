;;;; zpb-aws.asd

(asdf:defsystem #:zpb-aws
  :serial t
  :depends-on (#:drakma
               #:ironclad
               #:cxml
               #:cl-base64
               #:trivial-utf-8)
  :components ((:file "package")
               (:file "generic")
               (:file "credentials")
               (:file "signature")
               (:file "service")
               (:file "request")
               (:file "response")
               (:file "experiments")))

