
(in-package :motd)

(defparameter *preferred-languages* '(:en :fr :es :de :ja :zh))
;(setf *preferred-languages* '(:fr :en :es :de :ja :zh))

(defvar *motd-url* "http://motd.lisp.org/motds/most-recent")
;(setf *motd-url* "http://localhost:8000/motds/most-recent")

;;; This is :latin1 right now because that is what portable AllegroServe
;;; writes when not running under Allegro.
(defconstant +cache-external-format+ :latin1)
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

(defun load-cache ()
  (restore-cache-from-backup)
  (let ((*read-eval* nil))
    (with-open-file (*standard-input* *local-cache*
                                      :direction :input
                                      :element-type 'character
                                      :external-format +cache-external-format+
                                      :if-does-not-exist nil)
      (read *standard-input* nil))))

(defun motds-if-enough (contents)
  (if contents
      (destructuring-bind (requested &rest motds) contents
        (if (<= *messages-to-cache* requested)
            (values motds requested)
            (values nil 0)))
      (values nil 0)))

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

(defun join-strings (items separator)
  (apply #'concatenate 'string
         (rest (loop :for item :in items
                  :collecting separator
                  :collecting item))))

(defun print-tags (tags)
  (let* ((str (join-strings (mapcar #'symbol-name tags) ","))
         (pre-len (- 70 (length str))))
    (fresh-line)
    (write-string
     (subseq
      "----------------------------------------------------------------------"
      0
      (max 0 pre-len)))
    (write-string (string-downcase str))
    (write-string "--")
    (terpri)))

(defun print-motd-header ()
  (fresh-line)
  (write-string "Lisp Message of the Day")
  (terpri)
  (write-string
   "========================================================================")
  (terpri))

(defun find-best-translation (translations)
  (flet ((rank-language (language)
           (or (position language *preferred-languages*)
               (length *preferred-languages*))))
    (or (track-best:with-track-best (:order-by-fn #'<
                                                  :return-best t)
          (dolist (translation translations)
            (destructuring-bind (language . text) translation
              (track-best:track text (rank-language language)))))
        (cdr (first translations)))))

(defun print-motd (motd)
  (let* ((p-list (rest motd))
         (translations (getf p-list :translations))
         (tags (getf p-list :tags)))
    (write-string (find-best-translation translations))
    (terpri)
    (print-tags tags)))

(defun print-motds-to-string (display-at-most)
  (with-output-to-string (*standard-output*)
    (let* ((*messages-to-cache* (max display-at-most *messages-to-cache*))
           (all-motds (when (plusp display-at-most)
                        (load-or-fetch-motds)))
           (motds (subseq all-motds 0 (min (length all-motds)
                                           display-at-most))))
      (when motds
        (print-motd-header)
        (mapc #'print-motd motds)
        motds))))

(defun motd (display-at-most)
  (unless (quietp)
    (let ((terminal-io *terminal-io*))
      (flet ((retrieve-thread ()
               (let ((*terminal-io* terminal-io)
                     (output (print-motds-to-string display-at-most))
                     (thread (bt:current-thread)))
                 (when (plusp (length output))
                   (print-respecting-repl output
                                          (lambda ()
                                            (unless (eq (bt:current-thread)
                                                        thread)
                                              (bt:join-thread thread))))))))
        (bt:make-thread #'retrieve-thread :name "MOTD: RETRIEVE-THREAD"))))
  (values))
