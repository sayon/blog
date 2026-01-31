(require 'ox-publish)
(require 'subr-x)

(setq pubdir "~/public_html")
(setq basedir "~/repos/blog")

(defun my/link-gen (p fname label) (format "[ <a href=\"%s%s\"/> %s </a> ]" p fname label))
(defun my/preamble-gen (p) (concat
                            "<nav> <center>"
                            (my/link-gen p "/index.html" "Home")
                            (my/link-gen p "/about.html" "About")
                            (my/link-gen "https://github.com/sayon" "" "Github")
                            "</center> </nav>"
                            ))
(setq org-export-global-macros '(
                                 ("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")
                                 ("div" . "@@html:<div>$1</div>@@")
                                 ))
(defun my/org-collect-keywords (keywords filename &optional unique directory)
  (if filename
      (progn
        (when-let ((contents (org-file-contents filename :noerror)))
          (with-temp-buffer
            (insert contents)
            (org-collect-keywords keywords unique directory))
          ))))

(defun my/org-publish-find-keyword-or-empty (filename keyword) ""
  (let* ((keyword-list (my/org-collect-keywords (list keyword) filename))
         (excerpt (and keyword-list (nth 1 (car keyword-list)))))
    (or excerpt "")))


(defun my/org-publish-find-excerpt-or-empty (filename) ""
       (my/org-publish-find-keyword-or-empty filename "excerpt")
)


(defun my/org-sitemap-date-entry-format (entry style project) ""
       (let* ((filename (org-publish--expand-file-name entry project))
             (title (org-publish-find-title entry project))
             (is-draft (my/org-publish-find-keyword-or-empty filename "draft"))
             (post-date (org-publish-find-date entry project)))
         (if (string-empty-p is-draft)
             (if (string-empty-p title)
                 (format "*%s*" entry)
               (format "{{{timestamp(%s)}}} [[file:%s][%s]] \n {{{div(%s)}}}"
                       (format-time-string "%d-%m-%Y" post-date)
                       entry
                       title
                       (org-macro-escape-arguments
                        (my/org-publish-find-excerpt-or-empty filename))
                       ))
             ""
           )))



;;(advice-add 'org-export-output-file-name :around #'my/org-export-output-file-name-with-date)
;;
(defun vc-rename-current-file-basename (new-name)
  "Rename current file to NEW-NAME in the same directory, updating VC."
  (interactive
   (let* ((old (buffer-file-name)))
     (unless old
       (user-error "Current buffer is not visiting a file"))
     (list (expand-file-name
            (read-string "New name: " (file-name-nondirectory old))
            (file-name-directory old)))))
  (let* ((old (buffer-file-name))
         (backend (vc-backend old)))
    (when (buffer-modified-p)
      (save-buffer))
    (if backend
        ;; bypass 'update before move' check
        (vc-call-backend backend 'rename-file old new-name t)
      (rename-file old new-name))
    (set-visited-file-name new-name t t)))

(defun my/list-with-only-empty-strings-p (x)
  (and (listp x)
       (cl-every (lambda (e)
                   (and (stringp e)
                        (string-blank-p e)))
                 x)))

(defun my/org-publish-sitemap (title list)
  "Default site map, as a string.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
         (let* ((new-list (cl-remove-if #'my/list-with-only-empty-strings-p list))
                (result (concat "#+TITLE: " title "\n\n"
                                (org-list-to-org new-list))))
           result
           ))

(setq org-publish-project-alist
      (list
       (list "blog"
             :base-directory basedir
             :base-extension "org"
             :publishing-directory pubdir
             :recursive nil
             :publishing-function 'org-html-publish-to-html
             :author "Igor Zhirkov"
             :with-creator t
             :headline-levels 4
             :auto-preamble nil
             :with-creator t
             :html-doctype "html5"
             :html-html5-fancy t
             :html-head-include-scripts nil
             :html-head-include-default-style nil
             :auto-sitemap nil
             :html-head-extra "<link rel='stylesheet' type='text/css' href='css/site.css' />"
             :html-preamble (my/preamble-gen "." )
             :html-postamble nil
             )
       (list "blog-posts"
             :base-directory "posts"
             :base-extension "org"
             :publishing-directory (concat pubdir "/posts")
             :excluded '("setup" )
             :recursive t
             :publishing-function 'org-html-publish-to-html
             :auto-sitemap t
             :sitemap-title "rubber duck typing"
             :sitemap-filename "sitemap.org"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-style 'list
             :sitemap-function 'my/org-publish-sitemap
             :exclude-tags '("draft")
             :author "Igor Zhirkov"
             :with-creator t
             :headline-levels 4
             :auto-preamble t
             :with-creator t
             :html-doctype "html5"
             :html-head-include-default-style nil
             :html-html5-fancy t
             ;; :html-head-include-scripts nil
             ;; :css-name "css/site.css"
             :html-head-extra "<link rel='stylesheet' type='text/css' href='../css/site.css' />"
             :html-preamble (my/preamble-gen ".." )
             :sitemap-format-entry 'my/org-sitemap-date-entry-format
             :html-postamble nil
             :author "Igor Zhirkov"
             :with-creator t
             :exclude "setup.org"
             )
       (list "blog-static"
             :base-directory basedir
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
             :publishing-directory pubdir
             :excluded '("setup" "posts")
             :recursive t
             :publishing-function 'org-publish-attachment
             )
       (list "blog-all" :components '("blog-posts" "blog-static" "blog"))
       ))


(org-publish-all)
;;(save-window-excursion (async-shell-command "rsync -avLK ~/public_html/ rdt:~/public_html"))
