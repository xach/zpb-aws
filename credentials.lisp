;;;; credentials.lisp

(in-package #:zpb-aws)

(defvar *credentials* nil)

(defgeneric access-key (credentials))
(defgeneric secret-key (credentials))

(defmethod access-key ((credentials cons))
  (first credentials))

(defmethod secret-key ((credentials cons))
  (second credentials))
