;;;; mew-html.el
;;;;
;;;; display html email using web browser with header, inline images
;;;; and links to attachments
;;;;
;;;; redefined mew functions
;;;;   mew-mime-markup-language-ext in mew-mime.el
;;;;   mew-make-temp-name in mew-func.el

(require 'mew)

;;; save all multiparts for external display of HTML email

(defun mew-html-part-files-rec (boundary)
  ;; sub-function called by mew-html-save-all-multiparts
  ;; collect all multi-parts with names in the current buffer,
  ;; and make a list of (file-name start-point end-point)
  (let* ((p0 (when (re-search-forward (concat "^--" boundary "\r?$") nil t)
               (match-end 0)))
         (p0e (re-search-forward "^\r?$" nil t))
         (part-file (when (and p0 p0e)
                      (goto-char p0)
                      ;;(when (re-search-forward "Content-Type:[ -~\t\n\r]+name=\"\\([ -~]+\\)\"" p0e t)
                      (if (re-search-forward "Content-Location:[ \t\n\r]+\\([ -~]+\\)\r?$" p0e t)
                          (list (replace-regexp-in-string "%20" " " (match-string 1)) 'loc)
                        (if (re-search-forward "Content-Disposition: \\([^ ;]+\\) *;[ \t\n\r]+filename=\"\\([^\"]+\\)\"" p0e t)
                            (let ((type (match-string 1))
                                  (filename (match-string 2)))
                              (list (replace-regexp-in-string "%20" " " filename)
                                    (if (equal type "attachment")
                                        'attachment
                                      'loc)))))))
         (part-file-cid (when (and p0 p0e
                                   ;;(or (null part-file) (not (eql (nth 1 part-file) 'loc))))
                                   )
                          (goto-char p0)
                          (when (re-search-forward "Content-ID:[ \t\n\r]+<\\([^>@]+\\)\\(>\\|@\\)" p0e t)
                            (match-string 1)))))
    (when (and p0 p0e)
      (goto-char p0e)
      (let ((p1 (when (re-search-forward (concat "^--" boundary "\\(--\\|\\)\r?$") nil t)
                  (match-beginning 0))))
        (when p1
          (goto-char p1)
          (append
           (if part-file (list (list (car part-file) (1+ p0e) (1- p1) (cadr part-file))))
           (if part-file-cid (list (list part-file-cid (1+ p0e) (1- p1) 'cid)))
           (mew-html-part-files-rec boundary)))))))

(defun mew-html-find-all-boundary (cache)
  (let ((boundary-list '()))
    (save-excursion
      (set-buffer cache)
      (goto-char (point-min))
      (while (re-search-forward "Content-type:\\(.+;[ \t\n\r]*\\)+[ \t]*boundary=\"\\(.+\\)\"" nil t)
        (setq boundary-list (cons (match-string 2) boundary-list)))
      boundary-list)))

(defun mew-html-save-all-multiparts (cache file)
  "save all multi-parts with file names in cache buffer
to the directory where file is located.
Returns a list of attachments."
  (let ((dir (when (string-match "^\\(.+\\/\\)[^/]*\r?$" file)
               (match-string 1 file)))
        attachments)
    (save-excursion
      (set-buffer cache)
      (goto-char (point-min))
      (let ((all-files (apply 'append (mapcar '(lambda (boundary)
                                                 (mew-html-part-files-rec boundary))
                                              (mew-html-find-all-boundary cache)))))
        (dolist (part all-files attachments)
          (when (eql (nth 3 part) 'attachment)
            (setq attachments (cons (nth 0 part) attachments)))
          (mew-flet
           (write-region (nth 1 part) (nth 2 part) (concat dir (nth 0 part)) nil 'no-msg)))))))

(defun mew-html-header (cache)
  (save-excursion
    (set-buffer cache)
    (goto-char (point-min))
    (let ((pe (when (re-search-forward "^\r?$" nil t)
                (match-beginning 0)))
          (header '()))
      (dolist (key '("Subject" "From" "To" "Cc" "Date") (nreverse header))
        (let* ((p-head-s (progn (goto-char (point-min))
                                (re-search-forward (format "^%s: " key) pe t)))
               (p-head-e (when p-head-s (or (when (re-search-forward "\r?\n[^ :]+: " pe t)
                                              (match-beginning 0))
                                            pe))))
          (when p-head-e
            (setq header (cons (list key (buffer-substring-no-properties p-head-s p-head-e))
                               header))))))))

(defun mew-html-replace-html-chars (s)
  (dolist (rpl '(("&" "&amp;") ("<" "&lt;") (">" "&gt;") ("\"" "&quot;") ("\'" "&apos;")) s)
    (setq s (replace-regexp-in-string (car rpl) (cadr rpl) s))))

(defun mew-html-process-html (html-file &optional mail-header attachments)
  "change src file names of img tag in html-file"
  (save-excursion
    (set-buffer (find-file-noselect html-file))
    (goto-char (point-min))
    (perform-replace "<img \\([^>]*\\)src=\"?cid:\\([^@\"]+\\)@[^\"]+\"?"
                     "<img \\1 src=\"\\2\""
                     nil t nil)
    (goto-char (point-min))
    (perform-replace "<img \\([^>]*\\)src=\"?cid:\\([^ >\"]+\\)\"?"
                     "<img \\1 src=\"\\2\""
                     nil t nil)
    (when mail-header
      (let ((html-header (concat "<div style='background-color: #eeeeee;"
                                 "font-size:11.0pt;font-family:\"Calibri\",\"sans-serif\";'>"
                                 "<small><b>"
                                 (apply 'concat (mapcar '(lambda (h)
                                                           (format "%s: %s<br>"
                                                                   (car h)
                                                                   (mew-html-replace-html-chars (cadr h))))
                                                        mail-header))
                                 "</b></small></div><br>")))
        (goto-char (point-min))
        (cond ((re-search-forward "<body\\([^>]*\\)>" nil t)
               (goto-char (point-min))
               (perform-replace "<\\(body\\|BODY\\)\\([^>]*\\)>"
                                (concat "<body\\2>" html-header)
                                nil t nil))
              (t
               (insert html-header)))))
    (when attachments
      (let ((html-tail (concat "<hr>"
                               "<div style='font-size:11.0pt;font-family:\"Calibri\",\"sans-serif\";'>"
                               "<small>"
                               "<b>Attachments:</b><br>"
                               (apply 'concat (mapcar '(lambda (h)
                                                         (format "&nbsp;<a href=\"%s\">%s</a><br>" h h))
                                                      attachments))
                               "</small></div>")))
        (cond ((re-search-forward "</body>" nil t)
               (goto-char (match-beginning 0)))
              (t
               (goto-char (point-max))))
        (insert html-tail)))
    (save-buffer)
    (kill-buffer)))

;; redefinition of mew functions

;; redefinition of mew-mime-markup-language-ext in mew-mime.el

(defun mew-mime-markup-language-ext (program cache begin end params tag form)
  ;; called in Message buffer
  (when (> end begin)
    (let* ((file (format form (mew-make-temp-name "email")))
	   (prog (mew-progspec-get-prog program))
	   (args (mew-progsec-args-convert (mew-progspec-get-args program) file))
	   wcs
           (attachments (mew-html-save-all-multiparts cache file)))
      (save-excursion
	(message "Displaying %s..." tag)
	(set-buffer cache)
	;; charset check
	(setq wcs (mew-text/html-detect-cs begin end))
	;; note that application/xml may have the charset parameter
	(unless (mew-coding-system-p wcs)
	  (setq wcs (mew-charset-to-cs
		     (mew-syntax-get-param params "charset"))))
	(unless (mew-coding-system-p wcs)
	  (setq wcs mew-cs-text-for-write))
	(mew-frwlet mew-cs-dummy wcs
	  (write-region begin end file nil 'no-msg)
          (mew-html-process-html file (mew-html-header cache) attachments)
	  (apply 'mew-start-process-disp prog nil prog args))
	(message "Displaying %s...done" tag)))))

;;; redefinition of mew-make-temp-name in mew-func.el

(defun mew-make-temp-name (&optional fname)
  (unless (file-exists-p mew-temp-dir)
    (mew-make-directory mew-temp-dir)) ;; just in case
  (if fname
      (let ((temp-dir (make-temp-name mew-temp-file)))
        (mew-make-directory temp-dir)
        (expand-file-name fname temp-dir))
    (make-temp-name mew-temp-file)))

(provide 'mew-html)

;;; mew-html.el ends here
