(require 'ox-publish)
(require 'subr-x)

(setq pubdir (expand-file-name "~/public_html"))
(setq basedir (expand-file-name "~/repos/blog"))

;; Enable syntax highlighting for code blocks
(setq org-html-htmlize-output-type 'css)  ; Use CSS classes instead of inline styles
(setq org-src-fontify-natively t)         ; Fontify code blocks in org buffer

(defun my/link-gen (p fname label) (format "[ <a href=\"%s%s\"/> %s </a> ]" p fname label))
(defun my/preamble-gen (p) (concat
                            "<nav> <center>"
                            (my/link-gen p "/index.html" "Home")
                            (my/link-gen p "/about.html" "About")
                            (my/link-gen "https://github.com/sayon" "" "Github")
                            (my/link-gen p "/rss.xml" "RSS")
                            "[ <button class=\"theme-toggle\" onclick=\"toggleTheme()\" aria-label=\"Toggle dark mode\" title=\"Toggle dark/light mode\">"
                            "<span class=\"theme-icon\"></span>"
                            "</button> ]"
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

(defun my/publish-rss-file (plist filename pub-dir)
  "Publish RSS sitemap only, not individual files."
  (let ((filename-no-dir (file-name-nondirectory filename)))
    (if (string= filename-no-dir "rss.xml")
        (let ((output-file (expand-file-name filename-no-dir pub-dir)))
          (copy-file filename output-file t)
          output-file)
      nil)))

(defun my/get-org-keyword (file keyword)
  "Extract KEYWORD value from org FILE."
  (let ((keyword-list (my/org-collect-keywords (list keyword) file)))
    (when keyword-list
      (nth 1 (car keyword-list)))))

(defun my/org-file-to-html-string (file)
  "Convert org FILE to HTML string (body content only)."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-export-as 'html nil nil t)))

(defun my/format-rss-feed (title list)
  "Generate RSS feed from the sitemap LIST."
  (let ((base-url "https://rubber-duck-typing.com/posts/")
        (site-url "https://rubber-duck-typing.com/")
        (posts-dir (expand-file-name "posts" basedir))
        (rss-entries '()))

    ;; Collect RSS entries from org files
    (let ((org-files (directory-files posts-dir nil "\\.org$")))
      (dolist (filename org-files)
        (unless (string-match-p "\\(setup\\|sitemap\\)\\.org" filename)
          (let* ((file (expand-file-name filename posts-dir))
                 (post-title (my/get-org-keyword file "TITLE"))
                 (date-str (my/get-org-keyword file "DATE"))
                 (is-draft (my/get-org-keyword file "DRAFT"))
                 (link (concat base-url (file-name-sans-extension filename) ".html")))
            ;; Parse the date
            (when (and post-title date-str (not is-draft))
              (let ((date (org-time-string-to-time date-str))
                    (content (my/org-file-to-html-string file)))
                (push (list date
                           (concat
                            "    <item>\n"
                            "      <title>" (org-html-encode-plain-text post-title) "</title>\n"
                            "      <link>" link "</link>\n"
                            "      <guid>" link "</guid>\n"
                            "      <pubDate>" (format-time-string "%a, %d %b %Y %H:%M:%S %z" date) "</pubDate>\n"
                            "      <description><![CDATA[" content "]]></description>\n"
                            "    </item>\n"))
                      rss-entries)))))))

    ;; Sort by date (newest first) and concatenate
    (setq rss-entries (sort rss-entries (lambda (a b) (time-less-p (car b) (car a)))))
    (let ((items (mapconcat 'cadr rss-entries "")))
      (concat
       "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
       "<rss version=\"2.0\" xmlns:atom=\"https://www.w3.org/2005/Atom\">\n"
       "  <channel>\n"
       "    <title>" title "</title>\n"
       "    <link>" site-url "</link>\n"
       "    <description>A blog about programming, software engineering, and computer science</description>\n"
       "    <language>en-us</language>\n"
       "    <atom:link href=\"" site-url "rss.xml\" rel=\"self\" type=\"application/rss+xml\" />\n"
       items
       "  </channel>\n"
       "</rss>\n"))))

(setq org-publish-project-alist
      (list
       (list "blog"
             :base-directory (expand-file-name basedir)
             :base-extension "org"
             :publishing-directory (expand-file-name pubdir)
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
             :html-head-extra "<link rel='stylesheet' type='text/css' href='css/site.css' /><link rel='alternate' type='application/rss+xml' title='RSS Feed' href='rss.xml' /><script src='js/theme-toggle.js'></script>"
             :html-preamble (my/preamble-gen "." )
             :html-postamble nil
             )
       (list "blog-posts"
             :base-directory (expand-file-name "posts" basedir)
             :base-extension "org"
             :publishing-directory (expand-file-name "posts" pubdir)
             :exclude "setup"
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
             :html-head-extra "<link rel='stylesheet' type='text/css' href='../css/site.css' /><link rel='alternate' type='application/rss+xml' title='RSS Feed' href='../rss.xml' /><script src='../js/theme-toggle.js'></script>"
             :html-preamble (my/preamble-gen ".." )
             :sitemap-format-entry 'my/org-sitemap-date-entry-format
             :html-postamble nil
             :author "Igor Zhirkov"
             :with-creator t
             :exclude "setup.org"
             )
       (list "blog-static"
             :base-directory (expand-file-name basedir)
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
             :publishing-directory (expand-file-name pubdir)
             :exclude "posts\\|emacs"
             :recursive t
             :publishing-function 'org-publish-attachment
             )
       (list "blog-rss"
             :base-directory (expand-file-name "posts" basedir)
             :base-extension "org"
             :publishing-directory (expand-file-name pubdir)
             :publishing-function 'my/publish-rss-file
             :exclude "setup"
             :exclude-tags '("draft")
             :recursive nil
             :auto-sitemap t
             :sitemap-function 'my/format-rss-feed
             :sitemap-title "rubber duck typing"
             :sitemap-filename "rss.xml"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-style 'list
             )
       (list "blog-all" :components '("blog-posts" "blog-static" "blog" "blog-rss"))
       ))

;; Helper function to force republish from Emacs
(defun my/publish-blog-force ()
  "Publish the entire blog, forcing regeneration of all files."
  (interactive)
  (org-publish "blog-all" t))

;; Helper function for normal publish (respects timestamps)
(defun my/publish-blog ()
  "Publish the blog (only changed files)."
  (interactive)
  (org-publish "blog-all" nil))

;; Force republish all files, ignoring timestamps
(org-publish-all)
(save-window-excursion (async-shell-command "rsync -avLK ~/public_html/ rdt:~/public_html"))
