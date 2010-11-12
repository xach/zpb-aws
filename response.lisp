;;;; response.lisp

(in-package #:zpb-aws)

(defclass response ()
  ((status-code
    :initarg :status-code
    :accessor status-code)
   (reason-phrase
    :initarg :reason-phrase
    :accessor reason-phrase)
   (headers
    :initarg :headers
    :accessor headers)
   (content
    :initarg :content
    :accessor content)))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t)
    (format stream "~D ~S"
            (status-code response)
            (reason-phrase response))))
