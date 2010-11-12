;;;; generic.lisp

(in-package #:zpb-aws)

(defgeneric name (object))
(defgeneric url (object))
(defgeneric description (object))

(defgeneric request (object))
(defgeneric response (object))
(defgeneric service (object))

