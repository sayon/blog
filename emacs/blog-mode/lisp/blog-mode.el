;;; blog-mode.el --- Minor mode for editing blog posts -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Igor Zhirkov
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, writing
;; URL: https://github.com/sayon/blog-mode

;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;; A minor mode for editing blog posts on http://rubber-duck-typing.com with convenient features
;; and keybindings. Enable it using .dir-locals.el in your posts directory.

;;; Code:

(require 'magit)
(require 'org)
(require 'org-capture)
(require 'ox-publish)

(defgroup blog nil
  "Minor mode for editing blog posts."
  :group 'convenience
  :prefix "blog-")

(defcustom blog-posts-directory nil
  "Directory where blog posts are stored.
If nil, will try to detect automatically from buffer location."
  :group 'blog)

(defvar blog-mode-map
  (make-sparse-keymap)
  "Keymap for `blog-mode'.")

;;;###autoload
(define-minor-mode blog-mode
  "Minor mode for editing blog posts.
Automatically updates the filename to match the DATE property after save."
  :lighter " Blog"
  :keymap blog-mode-map
  :group 'blog-mode
    (if blog-mode
        (progn
          (add-hook 'after-save-hook #'blog-mode--maybe-update-date nil t)
          (message "Blog mode enabled"))
      (remove-hook 'after-save-hook #'blog-mode--maybe-update-date t)))

(defun org-find-keyword-or-empty (filename keyword) ""
  (let* ((keyword-list (org-collect-keywords (list keyword) filename))
         (excerpt (and keyword-list (nth 1 (car keyword-list)))))
    (or excerpt "")))


(defun blog-mode--maybe-update-date ()
  "Update filename to match DATE property if needed.
Called from `after-save-hook', so the file is already saved."
  (let* ((current-file (buffer-file-name))
         (dir (file-name-directory current-file))
         (basename (file-name-nondirectory current-file))
         ;; Remove existing YYYY-MM-DD- prefix if present
         (basename-no-date
          (replace-regexp-in-string "^\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-" "" basename))
         ;; Get date from org file
         (good-date (org-time-string-to-time (org-find-keyword-or-empty current-file "date")))
         (good-date-str (if good-date (format-time-string "%Y-%m-%d-" good-date) ""))
         ;; Construct new basename with date prefix
         (new-basename (if good-date (concat good-date-str basename-no-date) basename-no-date))
         (new-file (expand-file-name new-basename dir)))
    ;; Rename if changed
    (unless (string= current-file new-file)
      (magit-file-rename current-file new-file)
      (set-visited-file-name new-file t t))))

(defun blog-mode-update-date ()
  "Manually update the current blog post filename to match its DATE property.
Saves the buffer first if modified, then renames the file."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (when (buffer-modified-p)
    (save-buffer))
  (blog-mode--maybe-update-date))

(defun blog-mode-setup-capture-template (shortcut)
  (let ((template
         `(,shortcut "Blog post" plain
           (file (lambda ()
                   (let* ((slug (read-string "Post slug: "))
                          (date (format-time-string "%Y-%m-%d"))
                          (filename (concat date "-" slug ".org"))
                          (posts-dir blog-posts-directory))
                     (expand-file-name filename posts-dir))))
           "#+setupfile: setup.org\n#+title: %^{Title}\n#+date:<%<%Y-%m-%d %a>>\n#+excerpt: %^{Excerpt}\n\n%?"
           :empty-lines 0
           :jump-to-captured t
           :unnarrowed t)))
    ;; Remove existing blog capture template if present
    (setq org-capture-templates
          (assoc-delete-all shortcut org-capture-templates))
    ;; Add the new template
    (add-to-list 'org-capture-templates template)))



(provide 'blog-mode)

;;; blog-mode.el ends here
