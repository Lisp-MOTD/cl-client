(in-package #:motd-test)

(nst:def-test-group motd-time-helpers ()
  (nst:def-test hours (:equal (* 60 60))
    (motd::hours 1))

  (nst:def-test minutes (:equal 60)
    (motd::minutes 1))

  (nst:def-test seonds (:equal 1)
    (motd::seconds 1)))

(nst:def-test-group motd-quietp ()
  #+ccl
  (nst:def-test ccl-command-line-check-noinform (:not :true)
    (motd::ccl-has-quiet-flag-on-command-line '("-a" "--noinform" "-b")))

  #+ccl
  (nst:def-test ccl-command-line-check-Q (:true)
    (motd::ccl-has-quiet-flag-on-command-line '("-a" "-Q" "-b")))

  #+ccl
  (nst:def-test ccl-command-line-check-quiet (:true)
    (motd::ccl-has-quiet-flag-on-command-line '("-a" "--quiet" "-b")))

  #+ccl
  (nst:def-test ccl-command-line-check-batch (:true)
    (motd::ccl-has-quiet-flag-on-command-line '("-a" "--batch" "-b"))))
