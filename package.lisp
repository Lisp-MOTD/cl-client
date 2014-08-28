(defpackage #:motd
  (:use #:cl)
  (:export #:motd
           #:*preferred-languages*
           #:delete-cache
           #:*fetch-url-function*))
