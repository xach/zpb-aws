;;;; request.lisp

(in-package #:zpb-aws)

(defgeneric http-method (request))
(defgeneric http-verb (request))
(defgeneric request-uri (request))
(defgeneric parameters (request))
(defgeneric string-to-sign (request))
(defgeneric signature (request))
(defgeneric submit-request (request))
(defgeneric add-parameter (request name value))
(defgeneric (setf parameter) ( new-value request name))
(defgeneric signature-parameters (request))
(defgeneric make-request (service))

(defclass request ()
  ((http-method
    :initarg :http-method
    :accessor http-method)
   (request-uri
    :initarg :request-uri
    :accessor request-uri)
   (parameters
    :initarg :parameters
    :accessor parameters)
   (service
    :initarg :service
    :accessor service))
  (:default-initargs
   :http-method :get
   :request-uri "/"
   :parameters '()))

(defmethod http-verb ((request request))
  (string-upcase (http-method request)))

(defmethod add-parameter (request name value)
  (push (cons name value) (parameters request)))

(defmethod (setf parameter) (new-value request name)
  (let ((existing (assoc name (parameters request))))
    (if existing
        (setf (cdr existing) new-value)
        (prog1 new-value
          (push (cons name new-value) (parameters request))))))

(defvar *unreserved-character-bitmap*
  (let ((data (make-array 256 :element-type 'bit :initial-element 0)))
    (flet ((set-range (low high)
             (loop for i from low to high do (setf (aref data i) 1)))
           (set-values (&rest values)
             (dolist (i values)
               (setf (aref data i) 1))))
      (set-range 48 57)
      (set-range 65 90)
      (set-range 97 122)
      (set-values 45 46 95 126)
      data)))

(defun unreserved-character-code (code)
  (plusp (aref *unreserved-character-bitmap* code)))

(defun amazon-url-encode (string)
  (let ((octets (utf8 string))
        (*print-pretty* nil))
    (with-output-to-string (stream)
      (labels ((percent-encode (code)
                 (format stream "%~2,'0X" code))
               (output-code (code)
                 (if (unreserved-character-code code)
                     (write-char (code-char code) stream)
                     (percent-encode code))))
        (map nil #'output-code octets)))))

(defun iso8601-date-string (&optional (time (get-universal-time)))
  "Return a HTTP-style date string."
  (multiple-value-bind (second minute hour day month year day-of-week)
      (decode-universal-time time 0)
    (declare (ignore day-of-week))
    (let ((*print-pretty* nil))
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
              year month day hour minute second))))


(defmethod signature-parameters (request)
  (let ((params (sort (copy-list (parameters request)) #'string<
                      :key #'car)))
    (setf params (remove "Signature" params :test #'string= :key #'car))
    params))

(defmethod string-to-sign (request)
  (with-output-to-string (*standard-output*)
    (write-line (http-verb request))
    (write-line (signature-host (service request)))
    (write-line (request-uri request))
    (loop for ((name . value) . more) on (signature-parameters request)
          do
          (format t "~A=~A"
                  (amazon-url-encode name)
                  (amazon-url-encode value))
          (when more
            (format t "&")))))

(defmethod signature (request)
  (hmac-string (secret-key *credentials*) :sha256
               (string-to-sign request)))


