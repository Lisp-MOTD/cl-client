(in-package #:motd)

(defun ccl-has-quiet-flag-on-command-line
    (&optional (argv ccl:*command-line-argument-list*))
  (let* ((pos (position "--" argv :test #'string=))
         (argv (if pos
                   (subseq argv 0 pos)
                   argv)))
    (flet ((has-arg-p (arg)
             (member arg argv :test #'string=)))
      (some #'has-arg-p '("-Q" "--quiet" "--batch")))))

(defun impl-quietp ()
  (or ccl::*quiet-flag*
      ccl::*batch-flag*
      (ccl-has-quiet-flag-on-command-line)))
