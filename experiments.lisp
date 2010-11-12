;;;; experiments.lisp

(in-package #:zpb-aws)

(defun test-request (&key service (method :get) action arguments)
  (let* ((service (make-instance service))
         (request (make-instance 'request
                                 :service service
                                 :http-method method)))
    (add-parameter request "Action" action)
    (loop for (name value) on arguments by #'cddr do
          (add-parameter request name value))
    (add-parameter request "AWSAccessKeyId" (access-key *credentials*))
    (add-parameter request "SignatureMethod" "HmacSHA256")
    (add-parameter request "SignatureVersion" "2")
    (add-parameter request "Timestamp" (iso8601-date-string))
    (add-parameter request "Version" (api-version service))
    (add-parameter request "Signature" (signature request))
    (multiple-value-bind (content code headers uri stream must-close reason-phrase)
        (drakma:http-request (endpoint service)
                             :method (http-method request)
                             :want-stream nil
                             :parameters (parameters request))
      (declare (ignore uri))
      (when must-close
        (close stream))
      (make-instance 'response
                     :status-code code
                     :reason-phrase reason-phrase
                     :headers headers
                     :content (cxml:make-source content)))))
