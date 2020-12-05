;; -*- lexical-binding: t; no-byte-compile: t -*-
;; From https://opensource.com/article/20/3/blog-emacs,
;; Modified to use straight package manager
(setq invocation-name (file-name-nondirectory (car command-line-args)))
(setq comp-deferred-compilation nil)

;; Install straight
(defvar bootstrap-version)
(setq straight-repository-branch "develop")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(push "lib" straight-default-files-directive)
(straight-use-package 'use-package)

(use-package ox-publish)

(setq org-publish-project-alist
      '(("posts"
         :base-directory "posts/"
         :base-extension "org"
         :publishing-directory "docs/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :sitemap-title "Blog Index"
         :sitemap-filename "postlist.org"
         :sitemap-style list
         :author "Jacob Salzberg"
         :email "jssalzbe@ncsu.edu"
         :with-creator t)
        ("projects"
         :base-directory "projects/"
         :base-extension "org"
         :publishing-directory "docs/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :author "Jacob Salzberg"
         :email "jssalzbe@ncsu.edu"
         :with-creator t)
        ("css"
         :base-directory "css/"
         :base-extension "css"
         :publishing-directory "docs/css"
         :publishing-function org-publish-attachment
         :recursive t)
        ("photos"
         :base-directory "photos/"
         :base-extension "png\\|jpg\\|gif\\|svg\\|pdf"
         :publishing-directory "docs/photos"
         :publishing-function org-publish-attachment
         :recursive t)
        ("tetris"
         :base-directory "tetris/"
         :base-extension "json\\|js\\|gif\\|png\\|html\\|css\\|svg\\|gitignore"
         :publishing-directory "docs/tetris"
         :publishing-function org-publish-attachment
         :recursive t)
         ("all" :components ("posts" "projects" "css" "photos" "tetris"))))
