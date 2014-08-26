(in-package :motd)

(setf *fetch-url-function* 'ql-http-fetch-url)

(defun ql-http-fetch-url (url filename)
  (handler-case
      (multiple-value-bind (headers filename)
          (ql-http:fetch url filename
                         :if-exists :overwrite
                         :maximum-redirects 2)
        (declare (ignore headers filename))
        success)
    (ql-http:unexpected-http-status (err)
      (http-error (ql-http:unexpected-http-status-code err)))))
