(in-package :motd)

(defvar *fetch-url-function* nil
  "This variable holds a function designator for the function which
will be used to retrieve URLs.  It is a dynamic variable so that it
can be overridden for test functions.  The function takes two
arguments: the URL to retrieve and the FILENAME of the file in which
to write it.  The function must return an instance of
FETCH-URL-RETURN-TYPE.")

(deftype http-status () '(integer 100 599))

(adt:defdata fetch-url-return-type
  success
  (http-error http-status)
  (exception error))

(defun fetch-url (url filename)
  (handler-case
      (let ((result (funcall *fetch-url-function* url filename)))
        (check-type result fetch-url-return-type)
        result)
    (error (err)
      (exception err))))
