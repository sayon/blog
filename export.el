(require 'ox-publish)
(require 'ox-html)
(load-file "./ox-rss.el")


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
;; (defun my/org-sitemap-date-entry-format (entry style project) ""
;;        (let ((title (org-publish-find-title entry project)))
;;          (if (= (length title) 0)
;;              (format "*%s*" entry)
;;            (concat
;;             (format "{{{timestamp(%s)}}} [[file:%s][%s]] \n {{{div(%s)}}}"
;;                     (format-time-string "%d-%m-%Y"
;;                                         (org-publish-find-date entry project))
;;                     entry
;;                     title
;;                     (org-macro-escape-arguments
;;                      (my/org-publish-find-excerpt-or-empty (org-publish--expand-file-name entry project)))
;;                     )))))

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
             :base-extension "css\\|js\\|png\\|jpg\\|\\jpeg\\|gif\\|pdf\\|mp3\\|svg"
             :publishing-directory pubdir
             :excluded '("setup" "posts")
             :recursive t
             :publishing-function 'org-publish-attachment
             )
(list "rss"
     :base-directory "posts"
     :base-extension "org"
     ;; :rss-image-url "http://lumiere.ens.fr/~guerry/images/faces/15.png"
     :html-link-home "https://rubber-duck-typing.com"
     :html-link-use-abs-url t
     :rss-extension "xml"
     :publishing-directory pubdir
     :publishing-function 'org-rss-publish-to-rss
     :section-numbers nil
     :exclude ".*"            ;; To exclude all files...
     :include '("sitemap.org")   ;; ... except index.org.
     :table-of-contents nil)

      (list "blog" :components '("blog-posts" "blog-static" "rss" "blog"))
      ))

(org-publish-all)

(save-window-excursion (async-shell-command "rsync -avLK ~/public_html/ rdt:~/public_html"))

;; <feed xmlns="http://www.w3.org/2005/Atom" xml:lang="en">
;; <generator uri="https://www.gnu.org/software/emacs/" version="29">Emacs</generator>
;; <link href="http://rubber-duck-typing.com/feed.xml" rel="self" type="application/atom+xml"/>
;; <link href="https://rubber-duck-typing.com/" rel="alternate" type="text/html" hreflang="en"/>
;; <updated>2019-11-17T20:22:37-06:00</updated>
;; <id>https://www.rubber-duck-typing.com/</id>
;; <title type="html">Ted Kaminski</title>
;; <subtitle>Ted Kaminski's blog - a software design book writing project.</subtitle>
;; <author>
;; <name>Ted Kaminski</name>
;; <email>tedinski@cs.umn.edu</email>
;; <uri/>
;; </author>
;; <entry>
;; ...
;; </entry>
