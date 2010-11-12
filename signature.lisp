;;;; signature.lisp

(in-package #:zpb-aws)

(defun utf8 (string)
  (trivial-utf-8:string-to-utf-8-bytes string))

(defun hmac-string (key hash string)
  (let ((hmac (ironclad:make-hmac (utf8 key) hash)))
    (ironclad:update-hmac hmac (utf8 string))
    (cl-base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

