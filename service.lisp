;;;; service.lisp

(in-package #:zpb-aws)

(defgeneric endpoint (service))
(defgeneric signature-host (service))

(defclass service ()
  ((signature-host
    :initarg :signature-host
    :accessor signature-host)
   (endpoint
    :initarg :endpoint
    :accessor endpoint)
   (api-version
    :initarg :api-version
    :accessor api-version)
   (description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)))

(defclass dummy-service (aws-service)
  ((signature-host
    :reader signature-host
    :initform "dummy.xach.com")
   (endpoint
    :reader endpoint
    :initform "http://dummy.xach.com/foo")))

(defclass iam (service)
  ()
  (:default-initargs
   :endpoint "https://iam.amazonaws.com/"
   :signature-host "iam.amazonaws.com"
   :api-version "2010-05-08"
   :description "Identity and Access Management"))

(defclass simpledb (service)
  ()
  (:default-initargs
   :endpoint "https://sdb.amazonaws.com/"
   :signature-host "sdb.amazonaws.com"
   :api-version "2009-04-15"
   :description "SimpleDB"))

(defclass ec2 (service)
  ()
  (:default-initargs
   :endpoint "https://ec2.amazonaws.com/"
   :signature-host "ec2.amazonaws.com"
   :api-version "2010-08-31"
   :description "Elastic Compute Cloud"))

