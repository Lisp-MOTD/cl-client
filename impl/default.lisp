(in-package #:motd)

(defun impl-quietp ()
  "The default IMPL-QUIETP assumes we're never quiet."
  nil)

(defun print-respecting-repl (string &optional print-hook)
  "The default PRINT-RESPECTING-REPL just prints now because barring
  more information about the implementation, we cannot be careful to
  print at just the right spot in the REPL loop."
  (fresh-line *terminal-io*)
  (write-string string *terminal-io*)
  (when print-hook
    (funcall print-hook)))
