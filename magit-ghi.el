(require 'magit)
(require 'gh)
(require 'gh-issues)
(require 's)
(require 'browse-url)

(defun magit-gh-issues-get-api ()
  (gh-issues-api "api" :sync t :num-retries 1 :cache nil))

(defun magit-gh-issues-parse-url (url)
	(let* ((fixed-url (if (and (not (s-matches? "^[a-zA-Z_-]+://" url))
                             (s-matches? ":" url))
												(concat "ssh://" (s-replace ":" "/" url))
                      url))
				 (parsed-url (url-generic-parse-url fixed-url)))
		(let ((repo (s-match "/\\(.+\\)/\\([^/]+\\)/?$" (url-filename parsed-url))))
			(cons (cadr repo) (s-chop-suffix ".git" (cadr (cdr repo)))))))
  
(defun magit-gh-issues-guess-repo ()
  (let* ((remote (car (magit-git-lines "remote")))
				 (url (magit-get "remote" remote "url")))
		(magit-gh-issues-parse-url url)))

(defmacro magit-gh-issues-make-face (name col)
  (let ((face-name (intern (format "magit-gh-label-%s-face" (s-replace " " "-" name)))))
    `(defface ,face-name
       '((t :foreground ,col :box t))
       ,(concat "Face for GitHub label " name ".")
       :group 'magit-faces)))

(defun magit-gh-issues-get-issues ()
	(let* ((api (magit-gh-issues-get-api))
				 (repo (magit-gh-issues-guess-repo))
				 (user (car repo))
				 (proj (cdr repo)))
		(oref (gh-issues-issue-list api user proj) :data)))

(defun magit-gh-issues-get-labels ()
	(let* ((api (magit-gh-issues-get-api))
				 (repo (magit-gh-issues-guess-repo))
				 (user (car repo))
				 (proj (cdr repo)))
		(oref (gh-issues-label-list api user proj) :data)))
	  
(defun magit-gh-issues-visit-issue ()
  (interactive)
  (let ((url (magit-section-value (magit-current-section))))
    (when (yes-or-no-p (format "Would you like to open %s in a browser?" url))
      (browse-url url))))

(defvar magit-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-gh-issues-visit-issue)
    (define-key map [C-return] 'magit-gh-issues-visit-issue)
    map)
  "Keymap for `issues` section.")

(magit-define-section-jumper issues "Issues")

(defun magit-ghi-insert-ghi ()
	(let ((api (magit-gh-issues-get-api))
				(issues (magit-gh-issues-get-issues))
				(labels (magit-gh-issues-get-labels))
				(label-string nil))
		(when (> (length labels) 0)
			(dolist (label labels)
				(let* ((name (oref label :name))
							 (color (oref label :color)))
					(eval `(magit-gh-issues-make-face ,name ,(concat "#" color))))))
		(when (> (length issues) 0)
			(magit-insert-section (issues)
				(magit-insert-heading "Issues:")
				(dolist (issue issues)
					(let* ((label-string nil)
                 (id (oref issue :number))
								 (state (oref issue :state))
								 (labels (oref issue :labels))
								 (title (oref issue :title))
								 (open (string= "open" state))
								 (body (oref issue :body))
                 (url (oref issue :html-url)))
						(when labels
							(dolist (label labels)
								(let* ((name (cdr (assoc 'name label)))
											 (s (format "%s " (propertize name 'face (intern (format "magit-gh-label-%s-face" (s-replace " " "-" name)))))))
									(setq label-string (concat label-string s)))))
						(let ((heading
									 (format "%s\t%s %-10s\n"
                           (propertize (format "#%s" (number-to-string id))
                                       'face 'magit-tag)
                           title
                           (or label-string ""))))
              (when open
                (magit-insert-section (issue url t)
                  (magit-insert heading)
                  (magit-insert-heading)
                  (when body
                    (magit-insert-section (body)
                      (magit-insert (propertize
                                     (magit-gh-issues-format-text-in-rectangle
                                      (format "%s\n\n" (replace-regexp-in-string "[_`*]\\(.*?\\)[*`_]" "\\1"
                                                                                 (replace-regexp-in-string "```.*$" ""
                                                                                            (s-replace "" "" body)))) 120)
                                     'face 'magit-dimmed))
                        (magit-insert-heading)))))))))
				(when (> (length issues) 0)
					(insert "\n") t)))))

(defun magit-gh-issues-format-text-in-rectangle (text width)
	"Wrap a block of TEXT with a maximum WIDTH and indent."
	(with-temp-buffer
		(insert text)
		(goto-char (+ (point-min) width))
		(while (< (point) (point-max))
			(backward-word)
			(newline)
			(goto-char (+ (point) width)))
		(format "%s" (buffer-substring (point-min) (point-max)))))

(define-minor-mode magit-ghi-mode "GitHub Issues support for Magit using ghi"
	:lighter " ghi"
	:require 'magit-ghi
	(or (derived-mode-p 'magit-mode)
			(error "This mode only makes sense when used with magit"))
	(if magit-ghi-mode
			(magit-add-section-hook
			 'magit-status-sections-hook
			 'magit-ghi-insert-ghi
			 'magit-insert-stashes)
		(remove-hook 'magit-status-sections-hook 'magit-ghi-insert-ghi))
	(when (called-interactively-p 'any)
		(magit-refresh)))

(defun turn-on-magit-ghi ()
  "Unconditionally turn on `magit-ghi-mode`."
	(magit-ghi-mode 1))

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:
