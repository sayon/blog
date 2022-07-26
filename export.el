(require 'ox-publish)

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
                     (message filename)
                     (insert contents)
                     (org-collect-keywords keywords unique directory))
                   ))))

(defun my/org-publish-find-excerpt-or-empty (filename) ""
       (let ((lst (my/org-collect-keywords '("excerpt") filename)))
         (if lst (second (first lst)) "" )))

(defun my/org-sitemap-date-entry-format (entry style project) ""
       (let ((title (org-publish-find-title entry project)))
         (if (= (length title) 0)
             (format "*%s*" entry)
           (concat
            (format "{{{timestamp(%s)}}} [[file:%s][%s]] \n {{{div(%s)}}}"
                    (format-time-string "%d-%m-%Y"
                                        (org-publish-find-date entry project))
                    entry
                    title
                    (org-macro-escape-arguments
                     (my/org-publish-find-excerpt-or-empty (org-publish--expand-file-name entry project)))
                    )))))


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
       (list "blog" :components '("blog-posts" "blog-static" "blog"))
       ))


(org-publish-all)
(save-window-excursion (async-shell-command "rsync -avLK ~/public_html/ rdt:~/public_html"))

