;; -*- lexical-binding: t; -*-
(require 'org)
(require 'ox-publish)

(load-file "/Users/zzx/LocalDocs/mysite/templates.el")

(defvar my-site-root "" "Base path prefix for site-relative links and CSS.")

(defun my-publish-to-html (plist filename pub-dir)
  (org-publish-org-to 'my-html filename ".html" plist pub-dir))

(defun my/org-find-date (file project)
  "Find the DATE of FILE in PROJECT"
  (let* ((file (org-publish--expand-file-name file project))
         (date (org-publish-find-property file :date project)))
    (when-let* ((ts (and (consp date) (assq 'timestamp date)))
                (raw (org-element-interpret-data ts))
                (str (org-string-nw-p raw)))
      (org-time-string-to-time str))))


(defun my/org-sitemap-entry-format (entry style project)
  "Format sitemap ENTRY to link to .html and show its category."
  (cond
   ((not (directory-name-p entry))
    (let* ((title (org-publish-find-title entry project))
           (date (my/org-find-date entry project))
           (date-str (if date (format-time-string "%Y-%m-%d" date) ""))
           (entry-html (concat (file-name-sans-extension entry) ".html")))
      (format "%s [[file:%s][%s]]"
              date-str
              entry-html title)))
   (t
    (file-name-nondirectory (directory-file-name entry)))))

(defun my/sort-string-numeric-key (lst)
  "Sort LST by the numeric value of the string key (car of each element)."
  (sort lst (lambda (a b)
              (> (string-to-number (car a))
                 (string-to-number (car b))))))

(defun my/org-format-sitemap (list)
  "Convert grouped LIST into Org text with ** YEAR headings and - entries.
LIST has the shape (unordered (YEAR (unordered (DATE LINK)...))...)."
  (let ((result ""))
    (dolist (year-group (my/sort-string-numeric-key (cdr list)))  ;; skip top-level 'unordered
      (let ((year (car year-group))
            (posts (cdr year-group)))
        (setq result (concat result (format "** %s\n" year)))
        (dolist (entry (cdr (car posts)))  ;; skip inner 'unordered
            (setq result (concat result (format "- %s\n" (car entry)))))))
    result))

(defun my/org-publish-sitemap (title list)
  "Default site map, as a string.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
	        (my/org-format-sitemap list)))

(add-to-list 'org-export-options-alist
             '(:author "AUTHOR" nil nil newline))

(setq org-publish-project-alist
      '(
        ("about"
         :base-directory "."
         :base-extension "org"
         :publishing-directory "./public/"
         :with-toc nil
         :publishing-function my-publish-to-html
         :recursive t)

        ("blog"
         :base-directory "./blog"
         :base-extension "org"
         :publishing-directory "./public/blog"
         :sitemap t
         :sitemap-filename "index.org"
         :auto-sitemap t
         :recursive t
         :sitemap-title "Blog"
         :html-link-org-files-as-html t
         :html-inline-images t
         :sitemap-format-entry my/org-sitemap-entry-format
         :sitemap-sort-files anti-chronologically
         :sitemap-function my/org-publish-sitemap
         :publishing-function my-publish-to-html
         :recursive t)

        ("menu"
         :base-directory "./menu"
         :base-extension "org"
         :publishing-directory "./public/menu"
         :sitemap t
         :sitemap-filename "index.org"
         :auto-sitemap t
         :recursive t
         :sitemap-title "Menu"
         :html-link-org-files-as-html t
         :html-inline-images t
         :sitemap-format-entry my/org-sitemap-entry-format
         :sitemap-sort-files anti-chronologically
         :publishing-function my-publish-to-html
         :recursive t)

        ("lyrics"
         :base-directory "./lyrics"
         :base-extension "org"
         :publishing-directory "./public/lyrics"
         :sitemap t
         :sitemap-filename "index.org"
         :auto-sitemap t
         :recursive t
         :sitemap-title "Lyrics"
         :html-link-org-files-as-html t
         :html-inline-images t
         :sitemap-format-entry my/org-sitemap-entry-format
         :sitemap-sort-files anti-chronologically
         :publishing-function my-publish-to-html
         :recursive t)

        ("news"
         :base-directory "./news"
         :base-extension "org"
         :publishing-directory "./public/news"
         :with-toc nil
         :publishing-function my-publish-to-html
         :recursive t)
        
        ("bib"
         :base-directory "./bib"
         :base-extension "org"
         :publishing-directory "./public/bib"
         :with-toc t
         :publishing-function my-publish-to-html
         :recursive t)

        ("static"
         :base-directory "./assets/"
         :base-extension "css\\|js\\|jpg\\|jpeg"
         :publishing-directory "./public/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("my-site" :components ("about" "blog" "menu" "bib" "static" "news" "lyrics"))
        ))

(org-publish "my-site" t)
