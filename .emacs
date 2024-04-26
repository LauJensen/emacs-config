(add-to-list 'load-path "/home/lau/.emacs.d/elpa/")
(add-to-list 'load-path "/home/lau/.emacs.d/nano-emacs/")
(add-to-list 'exec-path "/home/lau/bin/")

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

(setq auto-mode-alist
      (append '(("/.lisp$" . lisp-mode)
                ("/.lsp$" . lisp-mode)
                ("/.cl$" . lisp-mode)
                ("//.java$" . java-mode)
                ("SConstruct$" . python-mode)
                ("/.py$" . python-mode)
                ("/.asd$" . lisp-mode)
                ("/.system$" . lisp-mode)
                ("//.org$" . org-mode)
                ("//.mbox$" . vm-mode)
                ("//.muse$" . muse-mode)
                ("//.htm$" . nxhtml-mumamo-mode)
                ("//.html$" . nxhtml-mumamo-mode)
                ("//.k8s$" . k8s-mode)
                ("//.d2" . d2-mode)
                ("//.clj$" . cider-mode))
              auto-mode-alist))

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

;(setq package-check-signature nil)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(when (not (package-installed-p 'use-package))
   (package-refresh-contents)
   (package-install 'use-package))

(use-package unicode-fonts       :ensure t)
(use-package nerd-icons          :ensure t)
(use-package doom-modeline       :ensure t)
(use-package jinx                :ensure t)
(use-package k8s-mode            :ensure t)
(use-package ejc-sql             :ensure t)
(use-package ivy                 :ensure t)
(use-package ivy-posframe        :ensure t :after ivy)
(use-package mood-line           :ensure t)
(use-package swiper              :ensure t)
(use-package yascroll            :ensure t)
(use-package auto-complete       :ensure t)
(use-package magit               :ensure t)
(use-package flycheck            :ensure t)
(use-package flycheck-clj-kondo  :ensure t)
(use-package indium              :ensure t)
(use-package d2-mode             :ensure t)
(use-package chatgpt-shell       :ensure t)
(use-package ranger              :ensure t)
(use-package subword             :ensure t)
(use-package idle-highlight-mode :ensure t)
(use-package pdf-tools           :ensure t)
(use-package svg-lib             :ensure t)
(use-package org-roam            :ensure t :after org)
(use-package org-autolist        :ensure t :after org)
(use-package forge               :ensure t :after magit)

;; With configs

(use-package markdown-mode
:ensure t
:mode ("README\\.md\\'" . gfm-mode)
:init (setq markdown-command "multimarkdown"))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package clojure-mode-extra-font-locking
:ensure t)

(use-package clojure-mode
  :ensure t
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

(use-package cider
  :ensure t
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
  :init
  (setq lsp-keymap-prefix "C-c C-u" ;; aids which-key
        gc-cons-threshold (* 100 1024 1024)
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
(use-package lsp-ui       :ensure t :commands lsp-ui-mode)
(use-package lsp-ivy      :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

(use-package org-autolist
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package org-roam
  :ensure t
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
  :ensure t
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-bullets
:ensure t
:after org
:hook (org-mode . org-bullets-mode))

(use-package org
  :mode (("\\.org$" . org-mode))
  :after org-fragtog
  :ensure t
  :init
  (define-key org-mode-map (kbd "<M-return>") nil)
  (global-unset-key        (kbd "<M-return>"))
  (global-set-key          (kbd "<M-return>") 'org-meta-return)
  :bind
  (("M-RET"   . org-meta-return)))


(use-package timu-rouge-theme
  :ensure t
  :config
  (load-theme 'timu-rouge t))

                                        ;(require 'unicode-fonts)
(require 'nerd-icons)
(require 'doom-modeline)
(require 'package)
(require 'k8s-mode)

(use-package company             :ensure t
    :init
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.8)) ;; default is 0.2

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(set-frame-font "iosevka 15")

(setq blink-cursor-blinks 0)

(set-face-attribute 'ivy-current-match nil :foreground "white" :background "red")

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

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

(use-package ob-d2 :ensure t :after d2-mode)

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
        (search-forward ":as")
        (when (> (current-column) max-col)
          (setq max-col (current-column)))
        (forward-line 1)))
    (message "max column %d" max-col)
    max-col))

(defun align-as ()
  (interactive)
  (if (use-region-p)
      (let ((start     (region-beginning))
            (end       (region-end))
            (rightmost (rightmost-as)))
        (message "The region is active, and is from %d to %d, padding to %d"
                 start end rightmost)
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (goto-char (line-beginning-position))
            (search-forward ":as")
            (backward-word)
            (backward-char)
            (when (< (current-column) rightmost)
              (setq _iter (- rightmost (current-column) 3))
              (while (> _iter 0)
                (insert " ")
                (setq _iter (- _iter 1))))
            (forward-line 1))))
    (message "nothing selected")))

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

(setq cider-repl-history-file "~/.cider-repl-history")

(setq cider-repl-use-pretty-printing t)

(setq flycheck-highlighting-mode 'sexps)

(require 'forge)

(defun approve-pr
    (pr-num)
  (interactive "sPull request number:")
  (if (= 0 (shell-command
            (concat "gh pr review " pr-num " -a")))
      (message "Approved")
    (message "Failed")))

(add-hook 'forge-topic-mode
          (lambda ()
            (local-set-key (kbd "C-c C-y") 'approve-pr)))

(add-to-list 'forge-alist
             (list "github.ccta.dk"
                   "api.github.ccta.dk"
                   "github.ccta.dk"
                   forge-github-repository))

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

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(require 'ivy-posframe)

(setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-posframe-display-at-window-center)
        (complete-symbol . ivy-posframe-display-at-window-center)
        (counsel-M-x     . ivy-posframe-display-at-window-center)
        (t               . ivy-posframe-display)))

(global-set-key (kbd "C-s") 'swiper)

(ivy-posframe-mode 1)

(load-theme 'deeper-blue)

(scroll-bar-mode 0)
(global-yascroll-bar-mode 1)

(require 'svg-tag-mode)

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (save-match-data
     (svg-image (svg-lib-concat
                 (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
                                   nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                 (svg-lib-tag (concat value "%")
                              nil :stroke 0 :margin 0)) :ascent 'center)))

  (defun svg-progress-count (value)
    (save-match-data
      (let* ((seq (split-string value "/"))
             (count (if (stringp (car seq))
                        (float (string-to-number (car seq)))
                      0))
             (total (if (stringp (cadr seq))
                        (float (string-to-number (cadr seq)))
                      1000)))
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count total) nil
                                          :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                    (svg-lib-tag value nil
                                 :stroke 0 :margin 0)) :ascent 'center))))

  (setq svg-tag-tags
        `(
          ;; Org tags
          (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))

          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


          ;; Citation of the form [cite:@Knuth:1984]
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                  (svg-tag-make tag
                                                                :end -1
                                                                :crop-left t))))


          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
           (,(format "\\(\\[%s\\]\\)" date-re) .
            ((lambda (tag)
               (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
           (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
            ((lambda (tag)
               (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
           (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
            ((lambda (tag)
               (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

          ;; ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))
          ))

(add-hook 'org-mode-hook
    (lambda ()
      (org-next-visible-heading 1)
      (svg-tag-mode 1)))

(doom-modeline-mode 1)
(add-hook 'after-init-hook #'doom-modeline-mode)
(setq mood-line-glyph-alist mood-line-glyphs-fira-code)
(set-face-attribute 'region nil :background "#666")

(set-face-background 'mode-line "#11131b")
(set-face-foreground 'mode-line "white")
(set-face-foreground 'mode-line-buffer-id "green")
(set-face-background 'mode-line-inactive "#1E1E1E")
(set-face-foreground 'mode-line-inactive "white")

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

(global-set-key (kbd "C-z") 'set-mark-command)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-æ æ") 'cider-repl-clear-buffer)

(global-set-key (kbd "C-M-p") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-o") 'shrink-window-horizontally)

(global-set-key (kbd "C-.") 'find-tag)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-æ c") 'qrt/wrap-in-comment-header)
(global-set-key (kbd "C-æ r") 'lsp-find-references)
(global-set-key (kbd "C-æ f") 'ranger)
