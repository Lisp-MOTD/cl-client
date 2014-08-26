(in-package :motd)

(setf *fetch-url-function* 'drakma-fetch-url)

(defun drakma-write-file (filename contents)
  (with-open-file (*standard-output* filename
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :overwrite
                                     :element-type (if (stringp contents)
                                                       'character
                                                       '(unsigned-byte 8)))
    (write contents))
  filename)

(defun drakma-fetch-url (url filename)
  (multiple-value-bind (contents status)
      (drakma:http-request url
                           :connection-timeout 2
                           :redirect 2)
    (cond
      ((= status 200)
       (success (drakma-write-file filename contents)))

      (t
       (http-error status)))))
