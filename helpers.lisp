(in-package #:motd)

(defun hours (n) (* n 60 60))
(defun minutes (n) (* n 60))
(defun seconds (n) n)

(defun quietp ()
  (impl-quietp))
