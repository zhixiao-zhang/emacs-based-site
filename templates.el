;; -*- lexical-binding: t; -*-
;; Extended Org → HTML exporter for Zhixiao's static site.
;; This file adds translators for most common Org elements,
;; so that exporting will preserve rich structure while keeping
;; the original minimalist CSS‑friendly markup.

(require 'ox-html)

;; -------------------------------------------------------------------
;; 1.  Define backend with an extended translate‑alist.
;; -------------------------------------------------------------------
(org-export-define-derived-backend 'my-html 'html
  :translate-alist '((template            . my/org-template)
                     (inner-template      . my/org-inner-template)
                     (headline            . my/org-headline)
                     (section             . my/org-section)
                     (paragraph           . my/org-paragraph)
                     (bold                . my/org-bold)
                     (italic              . my/org-italic)
                     (underline           . my/org-underline)
                     (strike-through      . my/org-strike-through)
                     (verbatim            . my/org-verbatim)
                     (code                . my/org-code)          ; inline =src=
                     (src-block           . my/org-src-block)
                     (example-block       . my/org-example-block)
                     (quote-block         . my/org-quote-block)
                     (plain-list          . my/org-plain-list)
                     (item                . my/org-item)
                     (link                . my/org-link)
                     (table               . my/org-table)
                     (line-break          . my/org-line-break)
                     (drawer              . my/org-drawer)
                     (property-drawer     . my/org-property-drawer)
                     (footnote-reference  . my/org-footnote-reference)
                     (footnote-definition . my/org-footnote-definition)))

;; -------------------------------------------------------------------
;; 2.  Generic helpers
;; -------------------------------------------------------------------
(defconst my/site-root (or (bound-and-true-p my-site-root) "./")
  "Site root for resolving absolute links.")

(defun my/org--maybe-add-class (tag class)
  "Return TAG (a string) optionally with CLASS attribute."
  (if class
      (format "%s class=\"%s\"" tag class)
    tag))

;; -------------------------------------------------------------------
;; 3.  Template and block‑level elements
;; -------------------------------------------------------------------
(defun my/org-template (contents info)
  (let* ((file (plist-get info :input-file))
         (active (cond
                  ((string-match-p "/news" file) "news")
                  ((string-match-p "/blog" file) "blog")
                  ((string-match-p "/menu" file) "menu")
                  ((string-match-p "/lyrics" file) "lyrics")
                  ((string-match-p "/bib" file) "bib")
                  (t "")))
         (head-extra (replace-regexp-in-string
                      "href=\"\\(css/\\|js/\\|assets/\\)"
                      (concat "href=\"" my-site-root "\\1")
                      (plist-get info :html-head))))
  (concat "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
          "<meta charset=\"utf-8\">\n"
          "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
          "<title>" (org-export-data (plist-get info :title) info) "</title>\n"
          "<link rel=\"stylesheet\" href=\"" my-site-root "css/basic-site-style.css\" />\n"
          head-extra
          "\n</head>\n<body>\n"
          "<header>\n<div class=\"container header-bar\">\n"
          "<div class=\"site-title\">Zhixiao Zhang</div>\n"
          "<nav>\n"
          "<div class=\"menu-icon\" id=\"menu-toggle\">☰</div>"
          "<ul class=\"menu hidden\">\n"
          "<li class=\"" (if (string= active "") "active" "") "\"><a href=\"" my-site-root "index.html\">About</a>\n</li>\n"
          "<li class=\"" (if (string= active "news") "active" "") "\">\n<a href=\"" my-site-root "news/index.html\">News</a>\n</li>"
          "<li class=\"dropdown\"" (if (string= active "blog") "active" "") "\">\n<a href=\"" my-site-root "blog/index.html\" class=\"deopbtn\">Blog</a>\n"
          "<div class=\"dropdown-content\"><a href=\"" my-site-root "menu/index.html\">Menu</a>\n"
          "<a href=\"" my-site-root "lyrics/index.html\">lyrics</a></div>\n</li>"
          "<li class=\"" (if (string= active "bib") "active" "") "\">\n<a href=\"" my-site-root "bib/paper.html\">Bib</a></li>\n"
          "</ul>\n</nav>\n</div>\n</header>\n"
          "<main>\n<div class=\"container\">\n"
          contents
          "</div>\n</main>\n"
          "<footer>\n<div class=\"footer-bar\">\n"
          "<p>&copy; 2025 Zhixiao Zhang · Powered by Emacs</p>"
          "</div>\n</footer>\n"
          "<script src=\"" my-site-root "js/script.js\" defer></script>\n"
          "</body>\n</html>")))

(defun my/old-org-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (let ((depth (plist-get info :with-toc)))
     (when depth (my/org-toc depth info)))
   contents
   (org-html-footnote-section info)))

(defun my/org-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist holding export options."
  (let* ((title  (org-export-data (plist-get info :title) info))
         (author (when (plist-get info :with-author)
                   (org-export-data (plist-get info :author) info)))
         (date   (when (plist-get info :with-date)
                   (org-export-data (plist-get info :date) info)))
         (toc    (let ((depth (plist-get info :with-toc)))
                   (when depth (my/org-toc depth info)))))
    (concat
     (format "<h1 class=\"center\">%s</h1>\n" title)
     (if author
         (format "<p class=\"center\">%s</p>\n" author)
       "")
     (if date
         (format "<p class=\"center\">%s</p>\n" date)
       "")
     toc
     contents
     (org-html-footnote-section info))))

(defun my/org-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let* ((toc-id-counter (plist-get info :org-html--toc-counter))
             (toc (concat (format "<div id=\"text-table-of-contents%s\" role=\"doc-toc\">"
                                  (if toc-id-counter (format "-%d" toc-id-counter) ""))
                          (org-html--toc-text toc-entries)
                          "</div>\n"))
             (toc-id (if toc-id-counter (format "-%d" toc-id-counter) "")))
        (plist-put info :org-html--toc-counter (1+ (or toc-id-counter 0)))
        (if scope
            toc
          (let ((outer-tag (if (org-html--html5-fancy-p info)
                               "nav" "div"))
                (heading-level (plist-get info :html-toplevel-hlevel)))
            (concat
             (format "<div id=\"toc-wrapper%s\" class=\"no-transition\">\n" toc-id)
             "<div id=\"toc-toggle-bar\" aria-label=\"Toggle TOC\"></div>\n"
             (format "<%s id=\"table-of-contents%s\" role=\"doc-toc\">\n" outer-tag toc-id)
             (format "<h%d>%s</h%d>\n"
                     heading-level
                     (org-html--translate "Table of Contents" info)
                     heading-level)
             toc
             (format "</%s>\n" outer-tag)
             "</div>\n")))))))


(defun my/org-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline. INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
           ;; `id' is kept internally, but not used in output
           (id (org-html--reference headline info))
           (formatted-text
            (if (plist-get info :html-self-link-headlines)
                (format "<a href=\"#%s\">%s</a>" id full-text)
              full-text)))
      (if (org-export-low-level-p headline info)
          ;; Render low-level headlines as list items.
          (let* ((html-type (if numberedp "ol" "ul")))
            (concat
             (and (org-export-first-sibling-p headline info)
                  (format "<%s>\n" html-type))
             (org-html-format-list-item
              contents (if numberedp 'ordered 'unordered)
              nil info nil
              (concat formatted-text)) "\n"
             (and (org-export-last-sibling-p headline info)
                  (format "</%s>\n" html-type))))
        ;; Render standard headline as a div with no attributes.
        (let ((first-content (car (org-element-contents headline))))
          (format "<%s id=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (format "%s" id)
                  (format "\n<h%d id=\"%s\">%s</h%d>\n"
                          level
                          id
                          formatted-text
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (org-element-type-p first-content 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))


(defun my/org-section (section contents _info)
  contents)

(defun my/org-paragraph (_paragraph contents _info)
  (format "<p>%s</p>\n" contents))

(defun my/org-bold (_bold contents _info)
  (format "<strong>%s</strong>" contents))

;; -------------------------------------------------------------------
;; 4.  Inline markup
;; -------------------------------------------------------------------
(defun my/org-italic (_italic contents _info)
  (format "<em>%s</em>" contents))

(defun my/org-underline (_underline contents _info)
  (format "<u>%s</u>" contents))

(defun my/org-strike-through (_strike contents _info)
  (format "<del>%s</del>" contents))

(defun my/org-verbatim (verbatim _contents info)
  "Handle inline =verbatim= code."
  (format "<code>%s</code>"
          (org-element-property :value verbatim)))

(defun my/org-code (code _contents info)
  "Handle inline ~code~ code."
  (format "<code>%s</code>"
          (org-element-property :value code)))

;; -------------------------------------------------------------------
;; 5.  Lists
;; -------------------------------------------------------------------
(defun my/org-plain-list (plain-list contents _info)
  "Export PLAIN-LIST element to <ul>/<ol> depending on its type."
  (let ((tag (if (eq (org-element-property :type plain-list) 'ordered) "ol" "ul")))
    (format "<%s>\n%s</%s>\n" tag contents tag)))

(defun my/org-item (_item contents _info)
  (format "<li>%s</li>\n" contents))

;; -------------------------------------------------------------------
;; 6.  Blocks
;; -------------------------------------------------------------------
(defun my/org-quote-block (_quote-block contents _info)
  (format "<blockquote>\n%s</blockquote>\n" contents))

(defun my/org-src-block (src-block _contents info)
  "Export SRC-BLOCK with language class for highlight.js (or similar)."
  (let* ((lang (downcase (or (org-element-property :language src-block) "")))
         (code (org-export-format-code-default src-block info)))
    (format "<pre class=\"src src-%s\">%s</pre>\n" lang code)))

(defun my/org-example-block (example-block _contents info)
  "Export EXAMPLE-BLOCK (indented example) without language class."
  (let ((code (org-export-format-code-default example-block info)))
    (format "<pre><span>%s</span></pre>\n" code)))

(defun org-html-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "<div class=\"center\">\n%s</div>" contents))

(defun my/org-table (_table contents _info)
  "Very simple HTML table export."
  (format "<div class=\"table-wrapper\">\n<table class=\"center\">\n%s</table>\n</div>\n" contents))

;; -------------------------------------------------------------------
;; 7.  Links
;; -------------------------------------------------------------------
(defun my/org-link (link desc info)
  "Strip leading assets/ from file links, then fallback to org-html-link."
  (let* ((raw-path (org-element-property :path link))
         (type     (org-element-property :type link)))
    (when (and (string= type "file")
               (string-prefix-p "assets/" raw-path))
      (org-element-put-property link :path (substring raw-path (length "assets/"))))
    (org-html-link link desc info)))




;; -------------------------------------------------------------------
;; 8.  Line breaks & drawers
;; -------------------------------------------------------------------
(defun my/org-line-break (&rest _args)
  "Hard line break \\  -> <br>."
  "<br />")

(defun my/org-drawer (_drawer contents _info)
  "Ignore drawer name, just output its CONTENTS."
  contents)

(defun my/org-property-drawer (_pd _contents _info)
  "Skip property drawers entirely."
  "")

;; -------------------------------------------------------------------
;; 9.  Footnotes
;; -------------------------------------------------------------------
(defun my/org-footnote-reference (footnote-reference contents info)
  (org-html-footnote-reference footnote-reference contents info))

(defun my/org-footnote-definition (footnote-definition contents info)
  (org-html-footnote-definition footnote-definition contents info))
