(in-package #:motd)

(defun ccl-has-quiet-flag-on-command-line
    (&optional (argv ccl:*command-line-argument-list*))
  "Check the command line for -Q or --quiet or --batch and return true
if any were specified."
  (let* ((pos (position "--" argv :test #'string=))
         (argv (if pos
                   (subseq argv 0 pos)
                   argv)))
    (flet ((has-arg-p (arg)
             (member arg argv :test #'string=)))
      (some #'has-arg-p '("-Q" "--quiet" "--batch")))))

(defun impl-quietp ()
  "There are some flags that might be set if CCL was started in quiet
mode.  Check those and check the command line for the arguments that
would have set those flags."
  (or ccl::*quiet-flag*
      ccl::*batch-flag*
      (ccl-has-quiet-flag-on-command-line)))

(defvar *motd-to-print* nil
  "When this variable is non-NIL, it will be printed after the next
  REPL eval.")

(defun print-respecting-repl (string)
  "The default PRINT-RESPECTING-REPL just prints now because barring
  more information about the implementation, we cannot be careful to
  print at just the right spot in the REPL loop."
  (flet ((print-it ()
           (when *motd-to-print*
             (fresh-line *terminal-io*)
             (write-string *motd-to-print* *terminal-io*)))
         (unregister ()
           (setf *motd-to-print* nil)
           (ccl:unadvise ccl::toplevel-eval
                         :when :after
                         :name :motd-print-respecting-repl)
           #+swank
           (ccl:unadvise swank::eval-region
                         :when :after
                         :name :motd-print-respecting-repl)))
    (setf *motd-to-print* string)
    (ccl:advise ccl::toplevel-eval
                (progn
                  (print-it)
                  (unregister))
                :when :after
                :name :motd-print-respecting-repl)
    #+swank
    (ccl:advise swank::eval-region
                (progn
                  (print-it)
                  (unregister))
                :when :after
                :name :motd-print-respecting-repl)))
