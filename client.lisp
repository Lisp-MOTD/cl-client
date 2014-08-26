
(in-package :motd)

(defvar *motd-url* "http://nklein.com/motd")

(defvar *local-cache* (merge-pathnames ".lisp-motd"
                                       (user-homedir-pathname)))
(defvar *cache-expiry* (hours 12))
(defvar *messages-to-cache* 10)

(defun cache-exists-p ()
  (probe-file *local-cache*))

(defun cache-date ()
  (if (cache-exists-p)
      (file-write-date *local-cache*)
      0))

(defun cache-expired-p ()
  (< (+ (cache-date) *cache-expiry*) (get-universal-time)))

(defun cache-backup-name ()
  (merge-pathnames ".lisp-motd-bak"
                   *local-cache*))

(defun careful-rename-file (old new)
  (when (probe-file old)
    (when (probe-file new)
      (delete-file new))
    (rename-file old new)))

(defun rename-cache-to-backup ()
  (careful-rename-file *local-cache* (cache-backup-name)))

(defun restore-cache-from-backup ()
  (careful-rename-file (cache-backup-name) *local-cache*))

(defun delete-cache ()
  ;; Need to delete the backup file, too, so we don't just
  ;; restore from it instead of fetching next time.
  (let ((backup (cache-backup-name)))
    (when (probe-file backup)
      (delete-file backup)))
  (when (cache-exists-p)
    (delete-file *local-cache*)))

(defun write-cache-file (contents)
  (with-open-file (*standard-output* (cache-backup-name)
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
    (write contents)))

(defun load-cache ()
  (restore-cache-from-backup)
  (let ((*read-eval* nil))
    (with-open-file (*standard-input* *local-cache*
                                      :direction :input
                                      :if-does-not-exist nil)
      (read *standard-input* nil))))

(defun motds-if-enough (contents)
  (when contents
    (destructuring-bind (requested &rest motds) contents
      (when (<= *messages-to-cache* requested)
        motds))))

(defun load-cached-motds ()
  (motds-if-enough (load-cache)))

(defun http-fetch-motds (url)
  (adt:match fetch-url-return-type (fetch-url url (cache-backup-name))
    (success
     (restore-cache-from-backup)
     (load-cached-motds))
    ((http-error code)
     (format *debug-io* "MOTD: failed to retrieve ~A (~A)~%" url code)
     nil)
    ((exception err)
     (format *debug-io* "MOTD: failed to retrieve ~A (~S)~%" url err))))

(defun fetch-motds ()
  (let ((url (format nil "~A/~D" *motd-url* *messages-to-cache*)))
    (http-fetch-motds url)))

(defun enough-motds-cached-p ()
  (multiple-value-bind (motds requested) (load-cached-motds)
    (when (<= *messages-to-cache* requested)
      motds)))

(defun load-or-fetch-motds ()
  (cond
    ((cache-expired-p)
     (fetch-motds))
    (t
     (or (load-cached-motds)
         (fetch-motds)))))

(defun print-motd-header ()
  (write-string
   "-Lisp-Message-of-the-Day------------------------------------------------")
  (terpri))

(defun print-motd-separator ()
  (write-string
   "------------------------------------------------------------------------")
  (terpri))

(defun print-motd (motd)
  (write-string motd)
  (fresh-line)
  (print-motd-separator))

(defun print-motds (display-at-most)
  (let* ((*standard-output* *terminal-io*)
         (*messages-to-cache* (max display-at-most *messages-to-cache*))
         (all-motds (when (plusp display-at-most)
                      (load-or-fetch-motds)))
         (motds (subseq all-motds 0 (min (length all-motds)
                                         display-at-most))))
    (when motds
      (print-motd-header)
      (mapcar #'print-motd motds)
      motds)))

(defun motd (display-at-most)
  (unless (quietp)
    (print-motds display-at-most)))
