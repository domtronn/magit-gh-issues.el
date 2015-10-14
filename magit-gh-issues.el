;;; magit-gh-issues.el --- GitHub issues extension for Magit

;; Copyright (C) 2015  Dom Charlesworth

;; Author: Dom Charlesworth <dgc336@gmail.com>
;; Keywords: git tools
;; Package-Version: 20150926.2354
;; Version: 0.0.1
;; URL: https://github.com/domtronn/magit-gh-issues
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

;;; Group Definitions
(defgroup magit-gh-issues nil
  "Customise the creation and display of GitHub issues in Magit."
  :group 'tools
  :group 'convenience)

(defcustom magit-gh-issues-ghi-executable (executable-find "ghi")
  "The executable with path for ghi.

By default, it performs `executable-find` to try and find ghi on your PATH."
  :group 'magit-gh-issues
  :type 'string)

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


(defvar magit-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-gh-issues-visit-issue)
    (define-key map [C-return] 'magit-gh-issues-visit-issue)
    (define-key map "a" 'magit-gh-issues-add-label)
    (define-key map "k" 'magit-gh-issues-remove-label)
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
      (let* ((name (cdr (assoc 'name label)))
             (s (format "%s"
                        (propertize
                         (format " %s " name)
                         'face (intern (magit-gh--build-face-name user proj name))))))
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

(defun magit-gh-issues-visit-issue ()
  "Get the URL meta data of the current issue and visit it in a browser."
  (interactive)
  (let ((url (cdr (assoc 'url (magit-section-value (magit-current-section))))))
    (when (y-or-n-p (format "Would you like to open %s in a browser?" url))
      (browse-url url))))

(defun magit-gh-issues-reload ()
  "Reload the issues by updating the cache of issues and labels.
It refreshes magit status to re-render the issues section."
  (interactive)
  (let ((repo (magit-gh-issues--guess-repo)))
    (if (not (and repo (car repo) (cdr repo)))
        (message "Remote repository is not configured or incorrect.")
      (magit-gh-issues-purge-cache)
      (magit-gh-issues-get-labels)
      (magit-gh-issues-get-issues)
      (magit-refresh))))


(defmethod magit-gh-issues--api-add-label ((api gh-issues-api) user repo
                                           issue-or-issue-id label)
  (let ((issue-id (gh-issues--issue-id issue-or-issue-id)))
    (gh-api-authenticated-request
     api (gh-object-list-reader (oref api label-cls)) "POST"
     (format "/repos/%s/%s/issues/%s/labels" user repo issue-id)
     (list label))))

(defmethod magit-gh-issues--api-remove-label ((api gh-issues-api) user repo
                                              issue-or-issue-id label)
  (let ((issue-id (gh-issues--issue-id issue-or-issue-id)))
    (gh-api-authenticated-request
     api (gh-object-list-reader (oref api label-cls)) "DELETE"
     (format "/repos/%s/%s/issues/%s/labels/%s" user repo issue-id label))))

(defun magit-gh-issues-start-ghi (&rest args)
  "Use magits process to run ghi ARGS with an editor."
  (run-hooks 'magit-pre-start-git-hook)
  (apply #'magit-start-process magit-gh-issues-ghi-executable nil
         (-flatten args)))

(defun magit-gh-issues-process-sentinel (process event)
  "Default sentinel used by `magit-start-process' around PROCESS and EVENT."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
      (message (concat (capitalize (process-name process)) " finished")))
    (magit-process-finish process)
    (when (eq process magit-this-process)
      (setq magit-this-process nil))
    (unless (process-get process 'inhibit-refresh)
      (let ((inhibit-magit-revert (process-get process 'inhibit-revert))
            (command-buf (process-get process 'command-buf)))
        (if (buffer-live-p command-buf)
            (with-current-buffer command-buf
              (magit-gh-issues-reload))
          (with-temp-buffer
            (setq default-directory (process-get process 'default-dir))
            (magit-gh-issues-reload)))))))

(define-derived-mode git-issue-mode nil "Git Issue"
  "Major mode for editing Git Issue files."
  :group 'magit
  (with-editor-mode 1)
  (git-commit-setup-font-lock))

(defun magit-gh-issues--reload-when-in-magit ()
  "Print message when back in magiit."
  (remove-hook 'after-change-major-mode-hook
               'magit-gh-issues--reload-when-in-magit))

(add-to-list 'auto-mode-alist '("GHI_ISSUE" . git-issue-mode))

(defun magit-gh-issues-open-issue ()
  "Open an issue using ghi."
  (interactive)
  (if magit-gh-issues-ghi-executable
      (let ((process
             (with-editor "GIT_EDITOR"
               (let ((magit-process-popup-time -1))
                 (magit-gh-issues-start-ghi "open")))))
        (set-process-sentinel process #'magit-gh-issues-process-sentinel))
    (error "Could not find the `ghi` executable.  Have you got it installed?")))

(defun magit-gh-issues-add-label ()
  "Add a label from a popup menu to the current issue and refresh."
  (interactive)
  (let ((labels (mapcar
                 (lambda (l) (list (cons 'name (format "%s" (gh-issues--label-name l)))))
                 (magit-gh-issues-get-labels))))
    (magit-gh-issues--call-label-api labels 'magit-gh-issues--api-add-label)))

(defun magit-gh-issues-remove-label ()
  "Remove a label from a popup menu to the current issue and refresh."
  (interactive)
  (let ((labels (cdr (assoc 'labels (magit-section-value (magit-current-section))))))
    (magit-gh-issues--call-label-api labels 'magit-gh-issues--api-remove-label)))

(defun magit-gh-issues--call-label-api (labels f)
  "Prompt LABELS and perform the API call F for the labels of the current issue."
  (when (eq 'issue (magit-section-type (magit-current-section)))
    (let* ((repo (magit-gh-issues--guess-repo))
           (prompt (magit-gh-issues--label-list labels (car repo) (cdr repo))))
      (let* ((label (replace-regexp-in-string "^ \\(.*\\) $" "\\1" (format "%s" (popup-menu* prompt :point (line-end-position)))))
             (issue (magit-section-value (magit-current-section)))
             (url (cdr (assoc 'url issue)))
             (id (cdr (assoc 'id issue))))
        (funcall f (magit-gh-issues--get-api) (car repo) (cdr repo) id label)
        (magit-gh-issues-reload)))))

(defun magit-gh-issues-insert-issues ()
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
          (let* ((id (oref issue :number))
                 (body (oref issue :body))
                 (url (oref issue :html-url))
                 (labels (oref issue :labels))
                 (label-string (when labels
                                 (magit-gh-issues--make-label-string labels user proj))))
            (magit-insert-section (issue `((url . ,url) (id . ,id) (labels . ,labels)) t)
              (magit-insert (magit-gh-issues--make-heading-string
                             id (oref issue :title) label-string))
              (magit-insert-heading)
              (when body
                (magit-insert-section (body)
                  (magit-insert (magit-gh-issues--make-body-string body))
                  (magit-insert-heading))))))
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
(define-key magit-status-mode-map (kbd "Io") 'magit-gh-issues-open-issue)

;;;###autoload
(define-minor-mode magit-gh-issues-mode "GitHub Issues support for Magit using gh"
  :lighter " ghi"
  :require 'magit-gh-issues
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense when used with magit"))
  (if magit-gh-issues-mode
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-gh-issues-insert-issues
       'magit-insert-stashes)
    (remove-hook 'magit-status-sections-hook 'magit-gh-issues-insert-issues))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(provide 'magit-gh-issues)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-gh-issues.el ends here
