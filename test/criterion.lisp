(in-package #:motd-test)

(nst:def-criterion (externalp (&optional (package "MOTD")) (symbol))
  (multiple-value-bind (sym externalp) (find-symbol (symbol-name symbol)
                                                    package)
    (declare (ignore sym))
    (case externalp
      (:external
       (nst:make-success-report))
      (nil
       (nst:make-failure-report
        :format "Unable to find symbol ~A in package ~A"
        :args (list symbol package)))
      (otherwise
       (nst:make-failure-report
        :format "Symbol ~A not external in package ~A"
        :args (list symbol package))))))

(nst:def-criterion (functionp (&optional (package "MOTD")) (symbol))
  (if (symbol-function (find-symbol (symbol-name symbol)
                                    package))
      (nst:make-success-report)
       (nst:make-failure-report
        :format "Symbol ~A in package ~A is not a function."
        :args (list symbol package))))

(nst:def-criterion (function-designator-p (&optional (package "MOTD")) (symbol))
  (let ((value (symbol-value (find-symbol (symbol-name symbol)
                                          package))))
    (cond
      ((or (functionp value)
           (and (symbolp value) (symbol-function value)))
       (nst:make-success-report))
      (t
       (nst:make-failure-report
        :format "Symbol ~A in package ~A is not a function designator."
        :args (list symbol package))))))
