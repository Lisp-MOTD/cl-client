(in-package #:motd-test)

(nst:def-test-group motd-exports ()
  (nst:def-test exports-motd (:all externalp
                                   functionp)
    :motd)

  (nst:def-test exports-delete-cache (:all externalp
                                           functionp)
    :delete-cache)

  (nst:def-test exports-fetch-url-function (:all externalp
                                                 function-designator-p)
    :*fetch-url-function*))
