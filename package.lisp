(defpackage #:motd
  (:use #:cl)
  (:export #:motd
           #:delete-cache
           #:*fetch-url-function*))
