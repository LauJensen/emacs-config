(add-to-list 'load-path "/home/lau/.emacs.d/elpa/")
(add-to-list 'load-path "/home/lau/.emacs.d/svg-tag-mode/")
(add-to-list 'exec-path "/home/lau/bin/")

(setq package-enable-at-startup nil)
(setq warning-minimum-level :error)

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setq my-backup-folder "/home/lau/Documents/EmacsBackups")
(setq my-exec-path     "/home/lau/bin")
(setq my-roam-folder   "~/Documents/Roam")

(setq browse-url-browser-function 'eww-browse-url)

(put 'erase-buffer 'disabled nil)
(autoload 'ido "ido.el")

(setq use-short-answers t)

(ido-mode 'both)  ; User ido mode for both buffers and files
(setq backup-directory-alist '(("." . "/home/lau/Documents/EmacsBackups")))
(setq x-select-enable-clipboard t) ; Integrate with X11s clipboard
(global-font-lock-mode 1) ;; Enable syntax highlighting when editing code.
(show-paren-mode 1) ; Highlight the matching paren
(tool-bar-mode -1)  ; Remove bloat
(menu-bar-mode -1)  ; --- || ---
(setq transient-mark-mode t) ; Highlight selected regions
(setq visible-bell t)        ; Flash program border on beep
(setq inhibit-startup-screen t) ; Dont load the about screen on load
(setq scroll-step 1)            ; Only scroll down 1 line at a time
(setq-default indent-tabs-mode nil) ; Dont indent with tabs
(column-number-mode t) ; Show cursors X + Y coordinates in modeline
(setq c-basic-offset 4) ; Indenting is 4 spaces
(set-language-environment "UTF-8");"Latin-1") ; Default would be utf8

(global-visual-line-mode t)

(setq scroll-step            1
      scroll-conservatively  10000)

(pixel-scroll-precision-mode)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (add-to-list 'exec-path my-exec-path))

(setq tab-always-indent 'complete)

(setq large-file-warning-threshold 1000000000)

(setq sentence-end-double-space nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/firefox")

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(setq
 backup-by-copying   t      ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions   6
 kept-old-versions   2
 version-control     t)

 (setq create-lockfiles nil)

;(setq package-check-signature nil)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(when (not (package-installed-p 'use-package))
   (package-refresh-contents)
   (package-install 'use-package))

(use-package unicode-fonts       :ensure (:wait t))
  (use-package nerd-icons          :ensure (:wait t))
  (use-package all-the-icons       :ensure (:wait t))
  (use-package doom-modeline       :ensure (:wait t))
  (use-package k8s-mode            :ensure (:wait t))
  (use-package ejc-sql             :ensure (:wait t))
  (use-package mood-line           :ensure (:wait t))
  (use-package yascroll            :ensure (:wait t))
  (use-package auto-complete       :ensure (:wait t))
  (use-package transient           :ensure (:wait t))
  (use-package magit               :ensure (:wait t) :after transient)
  (use-package flycheck            :ensure (:wait t))
  (use-package flycheck-clj-kondo  :ensure (:wait t))
  (use-package d2-mode             :ensure (:wait t))
  (use-package chatgpt-shell       :ensure (:wait t))
  (use-package ranger              :ensure (:wait t))
  (use-package idle-highlight-mode :ensure (:wait t))
  (use-package pdf-tools           :ensure (:wait t))
  (use-package svg-lib             :ensure (:wait t))
;;  (use-package forge               :ensure (:wait t) :after magit) currently an incompatability, magit it 4.20, forge requires 4.2.1

  ;; With configs

  (use-package markdown-mode
  :ensure (:wait t)
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

  (use-package jinx
    :ensure (:wait t)
    :hook (emacs-startup . global-jinx-mode)
    :bind (("M-$" . jinx-correct)
           ("C-M-$" . jinx-languages)))

  (use-package clojure-mode
    :ensure (:wait t)
    :mode (("\\.clj\\'" . clojure-mode)
           ("\\.cljs\\'" . clojure-mode)
           ("\\.cljd\\'" . clojure-mode)
           ("\\.cljc\\'" . clojure-mode)
           ("\\.edn\\'" . clojure-mode))
    :init
    (add-hook 'clojure-mode-hook #'subword-mode)
    (add-hook 'clojure-mode-hook #'clojure-mode-extra-font-locking)
    (add-hook 'clojure-mode-hook #'eldoc-mode)
    (add-hook 'clojure-mode-hook #'lsp)
    (add-hook 'clojure-mode-hook #'flycheck-mode)
    (add-hook 'clojure-mode-hook #'idle-highlight-mode))

  (use-package clojure-mode-extra-font-locking
  :after clojure-mode
  :ensure (:wait t))

  (use-package cider
    :ensure (:wait t)
    :after clojure-mode
    :defer t
    :diminish subword-mode
    :config
    (setq nrepl-log-messages t
          cider-repl-display-in-current-window t
          cider-repl-use-clojure-font-lock t
          cider-prompt-save-file-on-load 'always-save
          cider-font-lock-dynamically '(macro core function var)
          nrepl-hide-special-buffers t
          cider-overlays-use-font-lock t)
    (cider-repl-toggle-pretty-printing))

  (use-package lsp-mode
    :ensure (:wait t)
    :init
    (setq lsp-keymap-prefix "C-c C-u" ;; aids which-key
          gc-cons-threshold (* 100 1024 1024)
          lsp-headerline-arrow "=>"
          read-process-output-max (* 1024 1024)
          treemacs-space-between-root-nodes nil
          company-minimum-prefix-length 1
          lsp-idle-delay 0.800
          lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
                                          ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
          cider-eldoc-display-for-symbol-at-point t ; disable cider showing eldoc during symbol at point
          )
    :config
    (define-key lsp-mode-map (kbd "C-c C-u") lsp-command-map)
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
           (clojure-mode . lsp)
           ;; if you want which-key integration
           ;;(lsp-mode . lsp-enable-which-key-integration)
           )
    :commands lsp)

  ;; optionally
  (use-package lsp-ui       :ensure (:wait t) :commands lsp-ui-mode)
  (use-package lsp-treemacs :ensure (:wait t) :commands lsp-treemacs-errors-list)

  (use-package org-autolist
    :ensure (:wait t)
    :after org
    :hook (org-mode . org-autolist-mode))

  (use-package org-roam
    :ensure (:wait t)
    :init
    (setq org-roam-v2-ack t)
    (setq org-return-follows-link  t)
    (setq org-roam-node-display-template
          (concat "${title:*} "
                  (propertize "${tags:10}" 'face 'org-tag)))
    :custom
    (org-roam-directory my-roam-folder)
    (org-roam-completion-everywhere t)


    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           :map org-mode-map
           ("M-RET"   . org-meta-return)
           ("C-M-i"   . completion-at-point))
    :config
    (org-roam-setup))

  (use-package org-fragtog
    :ensure (:wait t)
    :after org
    :hook (org-mode . org-fragtog-mode))

  (use-package org-bullets
  :ensure (:wait t)
  :after org
  :hook (org-mode . org-bullets-mode))

  (use-package timu-rouge-theme
    :ensure (:wait t)
    :config
    (load-theme 'timu-rouge t))

  (use-package doom-themes
  :ensure (:wait t)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)
  (load-theme 'doom-Iosvkem t)
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


                                          ;(require 'unicode-fonts)
  (require 'nerd-icons)
  (require 'doom-modeline)
  (require 'package)
  (require 'k8s-mode)

(use-package company             :ensure (:wait t)
    :init
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.8)) ;; default is 0.2

(use-package corfu
  :ensure (:wait t)
  :init
  (global-corfu-mode))

(set-frame-font "iosevka 15")
(set-face-attribute 'default nil :weight 'light)

(setq blink-cursor-blinks 0)

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(setq frame-resize-pixelwise t)

(global-hl-line-mode 1)

(use-package org-tree-slide :ensure (:wait t))

(defun qrt/font-size (s)
  (interactive (list (read-number "font size: " 100)))
  (set-face-attribute 'default nil :height s))

(defvar qrt/org-meta-line-hidden-p nil)
(defun qrt/hide-org-meta-line ()
  (interactive)
  (setq qrt/org-meta-line-hidden-p t)
  (set-face-attribute 'org-meta-line nil
                      :foreground (face-attribute 'default :background)))

(defun qrt/show-org-meta-line ()
  (interactive)
  (setq qrt/org-meta-line-hidden-p nil)
  (set-face-attribute 'org-meta-line nil :foreground nil))

(defun qrt/toggle-org-meta-line-visibility ()
  (interactive)
  (if qrt/org-meta-line-hidden-p
      (qrt/show-org-meta-line)
    (qrt/hide-org-meta-line)))

(defvar qrt/orig-mode-line mode-line-format)

(defun qrt/hide-mode-line ()
  (setq-default mode-line-format nil))

(defun qrt/show-mode-line ()
  (setq-default mode-line-format qrt/orig-mode-line))

(defun qrt/toggle-mode-line ()
  (interactive)
  (if mode-line-format
      (qrt/hide-mode-line)
    (qrt/show-mode-line)))

(setq org-image-actual-width nil)
(setq org-tree-slide-activate-message "slideshow started")

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "<f8>") 'org-tree-slide-mode)))

(eval-after-load 'org-tree-slide
  (lambda ()
    (define-key org-tree-slide-mode-map (kbd "C-<right>") 'org-tree-slide-move-next-tree)
    (define-key org-tree-slide-mode-map (kbd "C-<left>") 'org-tree-slide-move-previous-tree)))

(defun qrt/init-org-tree-slide ()
  (org-bullets-mode 1)
  (org-toggle-inline-images 1)
  (qrt/hide-mode-line)
  (qrt/hide-org-meta-line)
  (qrt/font-size 200))

(defun qrt/finish-org-tree-slide ()
  (org-bullets-mode 0)
  (org-toggle-inline-images nil)
  (qrt/show-mode-line)
  (qrt/show-org-meta-line)
  (qrt/font-size 150))

(add-hook 'org-tree-slide-play-hook #'qrt/init-org-tree-slide)
(add-hook 'org-tree-slide-stop-hook #'qrt/finish-org-tree-slide)

(require 'ejc-sql)
;(require 'ejc-autocomplete)
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            ;(auto-complete-mode t)
            ;(setq ejc-set-column-width-limit nil)
            (ejc-set-column-width-limit nil)
            (ejc-ac-setup)))

(defun read-file (file)
  "Returns file as list of lines."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun pgpass-to-sql-connection (config)
  "Returns a suitable list for sql-connection-alist from a pgpass file."
  (let ((server (lambda (host port db user _pass)
                  (list
                   (concat db ":" user ":" port ":" host)
                   (list 'sql-product ''postgres)
                   (list 'sql-server host)
                   (list 'sql-user user)
                   (list 'sql-port (string-to-number port))
                   (list 'sql-database db))))
        (pgpass-line (lambda (line)
                       (apply server (split-string line ":" t)))))
    (mapcar pgpass-line config)))

(defun read-pgpass
    ()
  (let ((args (lambda (host port db user _pass _foo)
                (print host)
                (print port)
                (print db)
                (print (concat _pass ":" _foo))
                (ejc-create-connection
                 "PGPassed"
                 :classpath (concat "~/.m2/repository/org.postgresql/postgresql/42.6.0/"
                                    "postgresql-42.6.0.jar")
                 :subprotocol "postgresql"
                 :subname     (concat "//" host ":" port "/" db)
                 :user        user
                 :password    (concat _pass ":" _foo)))))
    (apply args (split-string (car (read-file "~/.pgpass")) ":" t))))

(defun pgpass-conn
    ()
  (interactive)
  (read-pgpass)
  (ejc-connect "PGPassed"))

(defun pg-local
    ()
  (interactive)
  (ejc-create-connection
   "PostgreSQL-db-connection"
   :classpath (concat "~/.m2/repository/org.postgresql/postgresql/42.6.0/"
                      "postgresql-42.6.0.jar")
   :subprotocol "postgresql"
   :subname "//localhost:5432/postgres"
   :user "postgres"
   :password "postgres")
  (ejc-connect "PostgreSQL-db-connection"))

(setq org-use-speed-commands t)

(setq org-startup-indented t)

(use-package ob-d2 :ensure (:wait t) :after d2-mode)

(require 'ob-d2)

(setq d2-location "~/.local/bin/d2")
(setq d2-output-format ".png") ;; Emacs sometimes chokes on svg

(org-babel-do-load-languages
    'org-babel-load-languages
    '((d2 . t)
      (scheme . t)))

(setq org-confirm-babel-evaluate nil)
(add-to-list 'image-types 'svg)

(setq org-src-tab-acts-natively nil)

(let* ((variable-tuple (cond ((x-list-fonts "iosevka 15") '(:font "iosevka 15"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'white))
       (headline           `(:inherit default :weight bold )))
  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.0))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.05))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.08))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.10))))
                          `(org-document-title ((t (,@headline ,@variable-tuple
                                                               :height 1.15
                                                               :underline nil))))))

(setq org-hide-emphasis-markers t)

(setq org-agenda-files '("~/Documents/TODOs/Opgaver.org"))

(setq org-todo-keywords
      '((sequence "TODO" "BLOCKED" "HOLD" "|" "DONE" "CANCELLED")))

(defvar find-file-root-prefix "/sudo:root@localhost:"
    "The prefix of root user use in Emacs.")

  (defun find-file-root (file)
    "Find file with root."
    (interactive "Find file as sudo: ")
    (find-file (concat find-file-root-prefix file)))

  (defun find-file-smb(file)
    "Access file through samba protocol."
    (interactive "fFind file as samba: ")
    (find-file (concat "/smb:" file)))

  (defun remove-dos-eol ()
    "Do not show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

  (defun get-string-from-file (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  (defun revert-all-buffers ()
    "Refresh all open file buffers without confirmation.
      Buffers in modified (not yet saved) state in emacs will not be reverted. They
      will be reverted though if they were modified outside emacs.
      Buffers visiting files which do not exist any more or are no longer readable
      will be killed."
    (interactive)
    (dolist (buf (buffer-list))
      (let ((filename (buffer-file-name buf)))
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers like *Messages*.
        (when (and filename
                   (not (buffer-modified-p buf)))
          (if (file-readable-p filename)
              ;; If the file exists and is readable, revert the buffer.
              (with-current-buffer buf
                (revert-buffer :ignore-auto :noconfirm :preserve-modes))
            ;; Otherwise, kill the buffer.
            (let (kill-buffer-query-functions) ; No query done when killing buffer
              (kill-buffer buf)
              (message "Killed non-existing/unreadable file buffer: %s" filename))))))
    (message "Finished reverting buffers containing unmodified files."))

(defun rightmost-as ()
    (interactive)
    (let ((start  (region-beginning))
          (end    (region-end))
          (max-col 0  ))
      (print "walking")
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (goto-char (line-beginning-position))
          (search-forward ":as" end t)
          (when (> (current-column) max-col)
            (setq max-col (current-column)))
          (forward-line 1)))
      (message "max column %d" max-col)
      (- max-col 3)))

(defun insert-spaces
   (n)
   (interactive)
   (while (> n 0)
      (insert " ")
      (setq n (- n 1))))

(defun align-as ()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end))
            (break nil)
            (rightmost (rightmost-as)))
        (message "The region is active, and is from %d to %d, padding to %d"
                 start end rightmost)
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (message "Point is %d" (point))
            (beginning-of-line)
            (if (or break (search-forward ":as" end t))
                (progn
                  (backward-word)
                  (backward-char)
                  (setq end (+ end (- rightmost (current-column))))
                  (insert-spaces (- rightmost (current-column)))
                  (forward-line 1)
                  (message "Point is %d" (point)))
              ;; If ":as" is not found, exit the loop
              (setq break t)))))
    (message "nothing selected")))

(defun align-namespace ()
  "Fixes :as keywords in requires before save-file "
  (interactive)
  (when (derived-mode-p 'clojure-mode)
    (save-excursion
      (goto-char (point-min))
      (mark-sexp)
      (align-as))
    (deactivate-mark)))

(add-hook 'write-file-hooks 'align-namespace)

(defun read-aws-creds (prefix profile)
  (interactive "Mprofile-name: ")
  (let ((on-profile-p nil))
    (with-temp-buffer
      (insert-file-contents "~/.aws/credentials")
      (while (not (eobp))
        (let ((line (buffer-substring (line-beginning-position)
                                      (line-end-position))))
          (when (and on-profile-p
                     (equal nil (cl-search "[" line)))
            (let* ((spl (split-string line " = "))
                   (env (upcase (car spl)))
                   (val (car (last spl))))
              (setenv env val)
              (message (concat env " :>> " val))))
          (when (cl-search "[" line)
            (if (cl-search (concat prefix "-" profile "]") line)
                (setq on-profile-p t)
              (setq on-profile-p nil)))
          (forward-line 1))))))

(defun qrt/wrap-in-comment-header ()
  "Takes the line at point, upcases it, and wraps it in a formatted
comment (lisp style, ie. with ;;). Can for example be used to
format a title for a section of code that is comming."
  (interactive)
  (let* ((title (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
         (len (length title))
         (beg)
         (end))
    (move-beginning-of-line 1)
    (kill-line)
    (insert "---" (make-string len ?-) "---\n")
    (insert "-- " (upcase title)       " --\n")
    (insert "---" (make-string len ?-) "---")
    (forward-line -2)
    (move-beginning-of-line nil)
    (setq beg (point))
    (forward-line 2)
    (move-end-of-line nil)
    (setq end (point))
    (comment-region beg end)))

(global-set-key (kbd "C-æ c") 'qrt/wrap-in-comment-header)

(defun find-first-nonexistent-filename (filename)
  (let ((i 1))
    (while (and (< i 101)
                (file-exists-p
                 (concat "~/Documents/Chats/" filename (number-to-string i) ".org")))
      (setq i (1+ i)))
    (concat "~/Documents/Chats/" filename (number-to-string i) ".org")))

(defun save-chat (title)
  (interactive "sTitle: ")
  (let ((filename (find-first-nonexistent-filename title)))
    (write-file filename)
    (kill-buffer (current-buffer))))

(setq cider-inject-dependencies-at-jack-in t)

(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer nil); 'only-in-repl)
(setq cider-auto-select-error-buffer nil)
(setq cider-repl-display-help-banner nil)

(setq clojure-toplevel-inside-comment-form t)

(setq cider-repl-history-file "~/.cider-repl-history")

(setq cider-repl-use-pretty-printing t)

(setq flycheck-highlighting-mode 'sexps)
(setq flycheck-highlighting-style nil)

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (bound-and-true-p lsp-mode)
              (setq-default
               post-command-hook
               (--filter (not (and (consp it)
                                   (eq (car it) 'closure)
                                   (not (-difference
                                         '(cancel-callback method buf hook workspaces id)
                                         (-map #'car (cadr it))))))
                         (default-value 'post-command-hook))))))

(defun find-api-key (filename search-string)
  "Find the line following the first occurrence of SEARCH-STRING in FILENAME."
  (if (file-exists-p filename)
      (with-temp-buffer
        ;; Insert the contents of the file into the temp buffer
        (insert-file-contents filename)
        ;; Search for the search string from the beginning
        (goto-char (point-min))
        (if (search-forward search-string nil t)
            ;; Found the string, now move to the beginning of the next line
            (let ((start (line-beginning-position 2))
                  (end (line-end-position 2)))
              (when (and start end)
                ;; Extract the line and return it
                (buffer-substring-no-properties start end)))
          ;; If the search string was not found, return nil
          nil))
    ""))

(setq chatgpt-shell-openai-key (find-api-key "~/.api-keys" "chatgpt"))
(setq dall-e-shell-openai-key  (find-api-key "~/.api-keys" "chatgpt"))

(use-package consult :ensure (:wait t))
(use-package vertico :ensure (:wait t) :init (vertico-mode))

(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "M-y") 'consult-yank-replace)
(global-set-key (kbd "C-æ t l") 'consult-theme)

(use-package orderless :ensure (:wait t)
:custom
(completion-styles '(orderless basic))
(completion-category-defaults nil)
(completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
   :ensure (:wait t)
   :init (marginalia-mode))

(load-theme 'deeper-blue)

(scroll-bar-mode 0)
(global-yascroll-bar-mode 1)

(doom-modeline-mode 1)
(add-hook 'after-init-hook #'doom-modeline-mode)
(setq mood-line-glyph-alist mood-line-glyphs-fira-code)
(set-face-attribute 'region nil :background "#666")

(set-face-background 'mode-line "#11131b")
(set-face-foreground 'mode-line "white")
(set-face-foreground 'mode-line-buffer-id "green")
(set-face-background 'mode-line-inactive "#1E1E1E")
(set-face-foreground 'mode-line-inactive "#a83800")
(set-face-foreground 'doom-modeline-buffer-file "#a83800")

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(setq enable-recursive-minibuffers t)

(setq org-format-latex-options
      '(:foreground default :background default
        :scale 2.0
        :html-scale 1.0
        :matchers
        ("begin" "$1" "$" "$$" "\\(" "\\[")))

(defconst journal-path "~/Documents/journal/")

(defun get-today-filename ()
  (concat journal-path (format-time-string "%d-%m-%Y") ".org"))

(defun today-journal ()
  "Open today's journal."
  (interactive)
  (let ((fname (get-today-filename)))
    (find-file fname)))

(keymap-global-set "C-x j" 'today-journal)

(defconst qrt/ss8ch-agent-socket-var "SSH_AUTH_SOCK")
(defconst qrt/ss8ch-agent-process-id "SSH_AGENT_PID")
(defconst qrt/ss8ch-agent-search-end "; export")

(defun qrt/ss8ch-find-var-value-in-agent-response
    (var-name response)
  "Takes a var-name and the response of calling `ssh-agent` in a
   shell environment. Finds the value for the given var-name in
   the given agent response."
  (save-match-data
    (if (string-match (concat var-name "=\\([^;]+\\)" qrt/ss8ch-agent-search-end)
                      response)
        (match-string 1 response))))

(defun qrt/ss8ch-ensure-agent ()
  "Checks if the environment contains the pid var for an ssh
   agent. If not so, starts an ssh-agent process and captures its
   output the configure the environment."
  (when (not (getenv qrt/ss8ch-agent-process-id))
    (let ((agent-response (shell-command-to-string "ssh-agent")))
      (setenv qrt/ss8ch-agent-socket-var
              (qrt/ss8ch-find-var-value-in-agent-response
               qrt/ss8ch-agent-socket-var
               agent-response))
      (setenv qrt/ss8ch-agent-process-id
              (qrt/ss8ch-find-var-value-in-agent-response
               qrt/ss8ch-agent-process-id
               agent-response)))
    (message "ss8ch ~ agent started")))

(defun qrt/ss8ch-handle-passphrase-request (process process-message)
  "Helper function to handle passphrase requests from the ssh-add
   process."
  (save-match-data
    (if (string-match "passphrase.*:\\s *\\'" process-message)
        (process-send-string process
                             (concat (read-passwd process-message) "\n"))
      (if (not (string-match "^\n+$" process-message))
          (message (concat "ss8ch ~ " (string-trim process-message)))))))

(defun qrt/ss8ch-find-private-ssh-keys-in (directory)
  "Returns a list of file paths under directory for private ssh
   keys."
  (remove nil (mapcar (lambda (file-name)
                        (save-match-data
                          (if (string-match "^\\([^.]+\\)\\.pub$" file-name)
                              (concat directory (match-string 1 file-name)))))
                      (directory-files directory))))

(defun qrt/ss8ch-add (key-file)
  "Checks if an agent is registered in the environment. If not
   so, an agent is started and registered. Then runs ssh-add to
   add a key to the running SSH agent, using the minibuffer to
   ask for the keys passphrase."
  (interactive
   (list (completing-read "Select ssh key to add: "
                          (qrt/ss8ch-find-private-ssh-keys-in "~/.ssh/"))))
  (qrt/ss8ch-ensure-agent)
  (let (process)
    (unwind-protect
        (progn
          (setq process (start-process  "ssh-add" nil
                                        "ssh-add" (expand-file-name key-file)))
          (set-process-filter process 'qrt/ss8ch-handle-passphrase-request)
          (while (accept-process-output process)))
      (if (eq (process-status process) 'run)
          (kill-process process)))))

(defun qrt/split-window-to-other-buffer-below
      ()
    (interactive)
    (delete-other-windows)
    (split-window-below)
    (other-window 1)
    (let ((switch-to-prev-buffer-skip 'visible))
      (switch-to-next-buffer)))

  (global-set-key (kbd "C-æ 2") 'qrt/split-window-to-other-buffer-below)

  (defun qrt/split-window-to-other-buffer-right
      ()
    (interactive)
        (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (let ((switch-to-prev-buffer-skip 'visible))
      (switch-to-next-buffer)))

  (global-set-key (kbd "C-æ 3") 'qrt/split-window-to-other-buffer-right)

(global-set-key (kbd "C-z") 'set-mark-command)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-æ æ") 'cider-repl-clear-buffer)

(global-set-key (kbd "C-M-p") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-o") 'shrink-window-horizontally)

(global-set-key (kbd "C-.") 'find-tag)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-æ c") 'qrt/wrap-in-comment-header)
(global-set-key (kbd "C-æ f") 'ranger)
(global-set-key (kbd "C-æ g") 'rgrep)
(global-set-key (kbd "C-æ r") 'lsp-find-references)
(global-set-key (kbd "C-æ s") 'lsp-ui-find-workspace-symbol)
(global-set-key (kbd "C-æ t") 'org-todo-list)

(global-set-key (kbd "C-s-p") 'org-todo)

(global-set-key (kbd "C-|") (lambda () (interactive)
                               (insert "\\")))

;(add-hook 'elpaca-after-init-hook (lambda () (setq warning-minimum-level :warning)))
