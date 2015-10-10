;;; magit-gh-issues.el --- GitHub issues extension for Magit

;; Copyright (C) 2015  Dom Charlesworth

;; Author: Dom Charlesworth <dgc336@gmail.com>
;; Keywords: git tools
;; Package-Version: 20150926.2354
;; Version: 0.0.1
;; URL: https://github.com/sigma/magit-gh-pulls
;; Package-Requires: ((emacs "24") (gh "0.9.1") (magit "2.1.0") (pcache "0.2.3") (s "1.6.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'magit)
(require 'gh)
(require 'gh-issues)
(require 's)
(require 'browse-url)

(defun magit-gh-issues--get-api ()
  "Get the `gh-issues-api` object."
  (gh-issues-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun magit-gh-issues--parse-url (url)
  "Parse a remote URL into a cons cell of ( username . reponame )."
	(let* ((fixed-url (if (and (not (s-matches? "^[a-zA-Z_-]+://" url))
                             (s-matches? ":" url))
												(concat "ssh://" (s-replace ":" "/" url))
                      url))
				 (parsed-url (url-generic-parse-url fixed-url)))
		(let ((repo (s-match "/\\(.+\\)/\\([^/]+\\)/?$" (url-filename parsed-url))))
			(cons (cadr repo) (s-chop-suffix ".git" (cadr (cdr repo)))))))
  
(defun magit-gh-issues--guess-repo ()
  "Attempt to guess the repo object using remote."
  (let* ((remote (car (magit-git-lines "remote")))
				 (url (magit-get "remote" remote "url")))
		(magit-gh-issues--parse-url url)))

(defmacro magit-gh-issues--make-face (name col)
  "Macro to make a face called NAME with foreground and box COL."
  (let ((face-name (intern name)))
    `(defface ,face-name
       '((t :foreground ,col :box t))
       ,(concat "Face for GitHub label " name ".")
       :group 'magit-faces)))

(defun magit-gh--build-face-name (user proj name)
  "Build the name for a label face from USER PROJ and NAME.

The format should be `magit-gh-user-repo-label-name-face`"
  (replace-regexp-in-string
   "[. ]" "-"
   (format "magit-gh-%s-%s-label-%s-face" user proj name)))

(defun magit-gh-issues-get-labels ()
  "Get the raw labels data from the gh library for repo."
	(let* ((api (magit-gh-issues--get-api))
				 (repo (magit-gh-issues--guess-repo))
				 (user (car repo))
				 (proj (cdr repo)))
		(oref (gh-issues-label-list api user proj) :data)))

(defun magit-gh-issues-get-issues ()
  "Get the raw issues data from the gh library for repo."
	(let* ((api (magit-gh-issues--get-api))
				 (repo (magit-gh-issues--guess-repo))
				 (user (car repo))
				 (proj (cdr repo)))
		(oref (gh-issues-issue-list api user proj) :data)))

(defun magit-gh-issues-reload ()
  "Reload the issues by clearing the cache and fetching both
the labels and the issues (which will set them in the cache).

It the refreshes magit status to re-render the issues section."
  (interactive)
  (let ((repo (magit-gh-issues--guess-repo)))
    (if (not (and repo (car repo) (cdr repo)))
        (message "Remote repository is not configured or incorrect.")
      (magit-gh-issues-purge-cache)
      (magit-gh-issues-get-labels)
      (magit-gh-issues-get-issues)
      (magit-refresh))))

(defun magit-gh-issues-purge-cache ()
  "Purge the cache of all items for a repo matching the current
repos user and repo name."
  (let* ((api (magit-gh-issues--get-api))
         (cache (oref api :cache))
         (repo (magit-gh-issues--guess-repo)))
    (pcache-map cache (lambda (k v)
                        (when (string-match
                               (format "/repos/%s/%s/" (car repo) (cdr repo))
                               (car k))
                          (pcache-invalidate cache k))))))

(defun magit-gh--cached-p (type api user proj)
  "Check whether a repo has a cache for TYPE in API under USER & PROJ."
    (let ((cache-repo (format "/repos/%s/%s/%s" user proj type))
          (cached? nil))
      (pcache-map (oref api :cache)
                  (lambda (key _) (when (equal (car key) cache-repo)
                               (setq cached? t))))
      cached?))

(defun magit-gh-issues-cached-p (api user proj)
  "Check whether issues have been cached in API for USER & PROJ."
  (magit-gh--cached-p "issues" api user proj))

(defun magit-gh-labels-cached-p (api user proj)
  "Check whether labels have been cached in API for USER & PROJ."
  (magit-gh--cached-p "labels" api user proj))
	  
(defun magit-gh-issues-visit-issue ()
  "Get the URL meta data of the current issue and visit it in a browser."
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

(defun magit-gh-issues--make-label-faces (labels user proj)
  "Create a face for each label in LABELS namespaced by USER & PROJ."
  (dolist (label labels)
    (let* ((name (magit-gh--build-face-name user proj (oref label :name)))
           (color (oref label :color)))
      (eval `(magit-gh-issues--make-face ,name ,(concat "#" color))))))

(defun magit-gh-issues--label-list (labels user proj)
  "Create a propertized list of LABELS using faces namespaced by USER & PROJ."
  (let ((result '()))
    (dolist (label labels)
      (message "%s" label)
      (let* ((name (cdr (assoc 'name label)))
             (s (format "%s"
                        (propertize
                         (format " %s " name)
                         'face (intern (magit-gh--build-face-name user proj name))))))
        (message "%s" result)
        (setq result (append result (list s)))))
    result))

(defun magit-gh-issues--make-label-string (labels user proj)
  "Create a propertized string of LABELS using faces namespaced by USER & PROJ."
  (mapconcat 'format (magit-gh-issues--label-list labels user proj) " "))

(defun magit-gh-issues--unmarkdown-body (body)
  "Remove markdown decoration like underscores from BODY."
  (let ((regexps '(("\\[\\(.*?\\)\\](.*?)" . "\\1")
                   ("[_`*]\\(.*?\\)[*`_]" . "\\1") 
                   ("```.*$" . "")
                   ("" . ""))))
    (mapc (lambda (regexp) (setq body (replace-regexp-in-string (car regexp) (cdr regexp) body))) regexps))
  (format "%s\n\n" body))

(defun magit-gh-issues--make-heading-string (id title &optional labels)
  "Create the propertized string used for issue headers using ID & TITLE.

If LABELS is non-nil, build the title will be appended with a propertized list
of labels specific to that GitHub project."
  (let ((id-p (propertize (format "#%s" (number-to-string id)) 'face 'magit-tag)))
    (format "%s\t%s %s\n" id-p title (or labels ""))))

(defun magit-gh-issues--make-body-string (body)
  "Create the propertized string used for the issue BODY."
  (propertize
   (magit-gh-issues-format-text-in-rectangle
    (magit-gh-issues--unmarkdown-body body) 120)
   'face 'magit-dimmed))

(defun magit-ghi-insert-ghi ()
  "Insert the actual issues sections into magit."
	(let* ((repo (magit-gh-issues--guess-repo))
         (user (car repo))
         (proj (cdr repo))
         (api (magit-gh-issues--get-api))
         (issues-cached? (magit-gh-issues-cached-p api user proj))
         (issues (when issues-cached? (magit-gh-issues-get-issues)))
         (labels-cached? (magit-gh-labels-cached-p api user proj))
         (labels (when labels-cached? (magit-gh-issues-get-labels)))
         (label-string nil))
    
		(when (> (length labels) 0)
      (magit-gh-issues--make-label-faces labels user proj))

    (when (or (not issues-cached?) (> (length issues) 0))
			(magit-insert-section (issues)
				(magit-insert-heading "Issues:")
				(dolist (issue issues)
					(let* ((body (oref issue :body))
                 (url (oref issue :html-url))
								 (open (string= "open" (oref issue :state)))
								 (labels (oref issue :labels))
                 (label-string (when labels
                                 (magit-gh-issues--make-label-string labels user proj))))
              (when open
                (magit-insert-section (issue url t)
                  (magit-insert (magit-gh-issues--make-heading-string
                                 (oref issue :number)
                                 (oref issue :title)
                                 label-string))
                  (magit-insert-heading)
                  (when body
                    (magit-insert-section (body)
                      (magit-insert (magit-gh-issues--make-body-string body))
                      (magit-insert-heading)))))))
        (when (> (length issues) 0)
					(insert "\n") t)
        (when (not issues-cached?)
					(insert "Fetch issues by pressing `I g`\n\n") t)))))

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

(define-key magit-status-mode-map (kbd "Ig") 'magit-gh-issues-reload)

;;;###autoload
(define-minor-mode magit-ghi-mode "GitHub Issues support for Magit using gh"
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

;;;###autoload
(defun turn-on-magit-ghi ()
  "Unconditionally turn on `magit-ghi-mode`."
	(magit-ghi-mode 1))

(provide 'magit-gh-issues)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-gh-issues.el ends here
