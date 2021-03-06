;; Author: Lancelot Zealot
;; Date: 2018-05-15 Tuesday 21:11:52 CST
;; File description: Emacs configuration

;; emacs package sources
(package-initialize)
(setq package-archives	       
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("gnu-cn" . "http://elpa.emacs-china.org/gnu/")
	("melpa-cn" . "http://elpa.emacs-china.org/melpa/")))

;; emacs config home directory
(setq emacs-home "D:\\emacs\\.emacs.d")

;; ensure packages that are not installed yet will be installed automatically
(setq use-package-always-ensure t)

(use-package ahungry-theme)

(if (display-graphic-p)
    (load-theme 'ahungry t) 		;t means no load theme confirm
  (load-theme 'tsdh-dark t))



;; turn off cursor's blink
(blink-cursor-mode -1)

;; youdao dictionary
(use-package youdao-dictionary
  :bind ("C-c y" . youdao-dictionary-search-at-point+))

;; can open minibuffers in minibuffers
(setq enable-recursive-minibuffers t)
;; display a list when opening files, M-x, describing functions and variables, searching files and using imenu
(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
	 ("M-x" . counsel-M-x)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)
	 ("C-x C-l" . counsel-locate)
	 ("M-s i" . counsel-imenu)))


;; display a list when searching strings
(use-package swiper
  :bind ("<f2>" . swiper))

;; dependency of counsel and swiper
;; what's more, it makes switch-to-buffer display a list
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

(use-package ag
  :config
  (setq ag-highlight-search t))

(setq scroll-margin 3
      scroll-conservatively 10000)

;; expand or contract selected region
(use-package expand-region
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))
;; js mode
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist
	       '("\\.js" . js2-mode)))
;; web mode
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist
	       '("\\.html" . web-mode)))



;; complete parentheses smartly, including () [] {} "" <> <%%>
(use-package smartparens
  :config
  (smartparens-global-mode t)
  (sp-with-modes '(web-mode nxml-mode)
    (sp-local-pair "<" ">")
    (sp-local-pair "<%" "%>")))
;; complete strings
(use-package company
  :bind ("M-/" . company-complete)
  :config
  (global-company-mode t))


;; emacs git
(use-package magit
  :bind ("C-x g" . magit-status))



;; code templates
(use-package yasnippet
  :config
  (add-hook 'ruby-mode-hook 'yas-minor-mode)
  (add-hook 'web-mode-hook 'yas-minor-mode)
  (add-hook 'js2-mode-hook 'yas-minor-mode)
  (add-hook 'C-mode-hook 'yas-minor-mode)
  (add-hook 'C++-mode-hook 'yas-minor-mode)
  (add-hook 'nxml-mode-hook 'yas-minor-mode)
  (add-hook 'java-mode-hook 'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode))

(use-package yasnippet-snippets
  :config
  (setq yas-wrap-around-region t))

;; powerful multiple cursors operations instead of iedit
(use-package multiple-cursors
  :bind (("C->"           . mc/mark-next-like-this)
	 ("C-<"           . mc/mark-previous-like-this)
	 ("C-M->"         . mc/skip-to-next-like-this)
	 ("C-M-<"         . mc/skip-to-previous-like-this)
	 ("C-c C-<"       . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)
	 ("C-;"           . mc/mark-all-symbols-like-this)))

;; project manager
(use-package projectile
  :config
  (setq projectile-completion-system 'default))

;; directory tree
;; (use-package neotree
;;   :bind ("<f9>" . neotree-toggle)
;;   :config
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; tree icons
;;(use-package all-the-icons)
;; dired mode icons
;;(use-package all-the-icons-dired
;;  :config
;;  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; clojure mode for .clj source files
(use-package clojure-mode
  :config
  (add-to-list 'auto-mode-alist
	       '("\\.clj" . clojure-mode)))
;; Clojure Interactive Development Environment that Rocks
(use-package cider)

;; export html with colorful code block
(use-package htmlize)

;; jump to specified char conveniently
(use-package avy
  :bind ("<f3>" . avy-goto-char))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (ace-window-display-mode t))

;; split windows with golden ratio
(use-package golden-ratio
  :bind ("<f12>" . golden-ratio))

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-display-duplicates nil)
  (setq browse-kill-ring-display-style (quote one-line))
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-highlight-inserted-item (quote solid))
  (setq browse-kill-ring-recenter nil)
  (setq browse-kill-ring-resize-window nil)
  (setq browse-kill-ring-show-preview nil)
  (setq browse-kill-ring-depropertize t)
  (setq browse-kill-ring-maximum-display-length 80)
  (setq browse-kill-ring-show-preview t)
  (setq kill-ring-max 20))


;; (use-package meghanada
;;   :config
;;   (add-hook 'java-mode-hook
;;             (lambda ()
;;               ;; meghanada-mode on
;;               (meghanada-mode t)
;;               (flycheck-mode +1)
;;               (setq c-basic-offset 4)
;;               ;; use code format
;;               (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))
;; 	    (cond
;; 	     ((eq system-type 'windows-nt)
;; 	      (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
;; 	      (setq meghanada-maven-path "mvn.cmd"))
;; 	     (t
;; 	      (setq meghanada-java-path "java")
;; 	      (setq meghanada-maven-path "mvn")))))

(use-package emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (define-key web-mode-map (kbd "<tab>") 'emmet-expand-line))

;; Highlight the cursor whenever the window scrolls
(use-package beacon
  :config
  (beacon-mode t)
  (setq beacon-color "red")
  (setq  beacon-blink-when-focused t))

;; enhanced zap-char
(use-package zzz-to-char
  :bind ("M-z" . zzz-to-char))

(use-package nyan-mode
  :config
  (nyan-mode t))



(spinner-start 'rotating-line)


(use-package drag-stuff
  :config
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))


(use-package highlight-thing
  :config
  (add-hook 'ruby-mode-hook 'highlight-thing-mode)
  (add-hook 'web-mode-hook 'highlight-thing-mode)
  (add-hook 'js2-mode-hook 'highlight-thing-mode)
  (add-hook 'C-mode-hook 'highlight-thing-mode)
  (add-hook 'C++-mode-hook 'highlight-thing-mode)
  (add-hook 'nxml-mode-hook 'highlight-thing-mode)
  (add-hook 'java-mode-hook 'highlight-thing-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-thing-mode))



(use-package typing)

(use-package rsense)
;; my awesome hydras!!
(use-package hydra
  :config
  (global-set-key (kbd "C-t")
		  (defhydra hydra-table (:color pink :hint nil)
		    "
^Ins/Del^			^Cell^		^Export^
------------------------------------------------------------
_i_: insert		_h_: heighten	_g_: generate source
_r_: insert row		_s_: shorten
_c_: insert column	_w_: widen
_d_: delete row		_n_: narrow
_D_: delete col		_j_: justify
			
   "
		    ("i" table-insert)
		    ("r" table-insert-row)
		    ("c" table-insert-column)
		    ("d" table-delete-row)
		    ("D" table-delete-column)
		    ("h" table-heighten-cell)
		    ("s" table-shorten-cell)
		    ("w" table-widen-cell)
		    ("n" table-narrow-cell)
		    ("j" table-justify)
		    ("g" table-generate-source :color blue)
		    ("q" nil "quit" :color blue))))


(setq hippie-expand-try-function-list '(try-expand-debbrev
                                        try-expand-debbrev-all-buffers
                                        try-expand-debbrev-from-kill
                                        try-complete-file-name-partially
                                        try-complete-file-name
                                        try-expand-all-abbrevs
                                        try-expand-list
                                        try-expand-line
                                        try-complete-lisp-symbol-partially
                                        try-complete-lisp-symbol))
;;(global-set-key (kbd "M-/") 'hippie-expand)

;; use ibuffer to take the place of bufferList
(global-set-key (kbd "C-x C-b") 'ibuffer)



;; list recently open files with C-x C-r
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; open recent files once emacs starts
(recentf-open-files)

;; display line number
(global-linum-mode t)
;; display column number with the style of (line number, column number)
(setq column-number-mode t)
;; turn off tool bar at the top
(tool-bar-mode -1)
;; turn off scroll bar
(scroll-bar-mode -1)
;; turn off menu bar at the top
(menu-bar-mode -1)
;; delete selected region when press any buttons instead of appending contents
(delete-selection-mode t)
;; hightlight paired parentheses
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; hightlight paired parentheses that is the nearest from the current cursor
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))
;; hightlight current line
(global-hl-line-mode t)
;; display time at minibuffer
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; directly find the selected line in occur mode
(defun occur-dwim ()
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym))))
	regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)





;; save desktop's layout when exiting emacs
;; (desktop-save-mode t)

;; only one buffer when open file in dired mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))
(put 'dired-find-alternate-file 'disabled nil)
;; always execute recursively when deleting and copying directories in dired mode
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)


;; transparent background
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; no welcome buffer when opening emacs
(setq inhibit-splash-screen t)
;; open this config file with F1
(global-set-key (kbd "<f1>")
		'(lambda ()
		   (interactive)
		   (find-file (expand-file-name "init.el" emacs-home))))
;; disable backing up files
(setq make-backup-files nil)
;; disable auto-save
(setq auto-save-default nil)
;; open emacs full screen
;; (setq initial-frame-alist (quote ((fullscreen . maximized))))

;; locate the files of functions, variables and key-bindings
(global-set-key "\C-h\ \C-f" 'find-function)
(global-set-key "\C-h\ \C-v" 'find-variable)
(global-set-key "\C-h\ \C-k" 'find-function-on-key)
;; indent the whole buffer
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(defun indent-region-or-buffer ()
  (interactive)
  list-faces-sample-text  (save-excursion
			    (if (region-active-p)
				(progn
				  (indent-region (region-beginning) (region-end))
				  (message "Indenting region...done"))
			      (progn
				(indent-buffer)
				(message "Indenting buffer...done")))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
;; replace yes or no with y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; enable code hightlight in org mode
(setq org-src-fontify-natively t)
;; move among windows with C-up C-down C-left C-right
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)
(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-right>") 'windmove-right)
;; cache accessed urls
(setq url-automatic-caching t)
;; turn off error bell and blink
(setq visible-bell nil
      ring-bell-function 'ignore)
;; emacs's inner copying and cutting will be effective with OS's clipboard
(setq select-enable-clipboard t)

;; start defining a macro
(global-set-key (kbd "<f7>") 'kmacro-start-macro-or-insert-counter)
;; end defining a macro or execute the current macro
(global-set-key (kbd "<f8>") 'kmacro-end-or-call-macro)
;; close current both buffer and window
(global-set-key (kbd "<f4>") 'kill-buffer-and-window)
;; copy current line regardless of the cursor's position
(global-set-key "\C-j"
		(lambda ()
		  (interactive)
		  (progn
		    (kill-whole-line)
		    (yank)
		    (message "copied line"))))
;; delete current line regardless of the cursor's position
(global-set-key (kbd "C-k") 'kill-whole-line)
;; query and replace with confirmation
(global-set-key (kbd "C-r") 'query-replace)
;; wrap line when the number of a line's chars is greater than 80, need to be in use with M-q or auto-fill-mode
(setq default-fill-column 80)
;; set in text mode when opening new files
(setq default-major-mode 'text-mode)
;; set a jump mark with C-, can jump back with C-. just like Vim's command m
(global-set-key (kbd "C-,")
		'(lambda ()
		   (interactive)
		   (point-to-register ?\`))) ;use register `
(global-set-key (kbd "C-.")
		'(lambda ()
		   (interactive)
		   (jump-to-register ?\`)))
;; save the current window layout
(global-set-key (kbd "<f5>")
		'(lambda ()
		   (interactive)
		   (window-configuration-to-register ?\~)
		   (message "Saving window layout...done")))
;; restore the window layout saved before
(global-set-key (kbd "<f6>")
		'(lambda ()
		   (interactive)
		   (jump-to-register ?\~)
		   (message "Restoring window layout...done")))

;; config emails, need .authinfo
(setq user-full-name "Lancelot Zealot")
(setq user-mail-address "18879538430@163.com")
(setq smtpmail-smtp-server "smtp.163.com")
(setq smtpmail-smtp-service 25)
(setq send-mail-function ''smtpmail-send-it)
;; set the default search engine of emacs's inner browser EWW
(setq eww-search-prefix "https://www.bing.com/search?q=")



;; set a macro to copy the current word at the point
(fset 'copy-word-at-point
      [?\C-= ?\M-w])
(global-set-key (kbd "C-`") 'copy-word-at-point)

(fset 'newline-at-any-point
      [?\C-e return])
(global-set-key (kbd "C-o") 'newline-at-any-point)

;; mouse will move away when the cursor meets it.
(mouse-avoidance-mode 'animate)

;; emacs can open pictures
(auto-image-file-mode)


;; enlarge and shrink current window
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)


;; repeat most recently executed command.
(global-set-key (kbd "<f9>") 'repeat)

;;(load "D:\\emacs\\.emacs.d\\lisp\\my-abbrev")
(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '(
    ;; my own frequently used abbrevs
    ("thx" "thanks")

    ;; net abbrev
    ("afaik" "as far as i know" )
    ("atm" "at the moment" )
    ("dfb" "difference between" )
    ("ty" "thank you" )
    ("ui" "user interface" )
    ("uns" "understand" )
    ("ur" "you are" )
    ("btw" "by the way" )
    ("cnt" "can't" )
    ("ddnt" "didn't" )
    ("dnt" "don't" )
    ;; english word abbrev
    ("ann" "announcement" )
    ("arg" "argument" )
    ("autom" "automatic" )
    ("bc" "because" )
    ("bg" "background" )
    ("bt" "between" )
    ("math" "mathematics" )
    ;; computing
    ("ahk" "AutoHotkey" )
    ("cfg" "context-free grammar" )
    ("cj" "Clojure" )
    ("cs" "computer science" )
    ;; tech company
    ("gc" "Google Chrome" )
    ("gm" "Google Map" )
    ("macos" "Mac OS" )
    ("msw" "Microsoft Windows" )
    ;; programing
    ("ev" "environment variable" )
    ("ipa" "IP address" )
    ("jvm" "Java Virtual Machine" )
    ("rsi" "Repetitive Strain Injury" )
    ("subdir" "sub-directory" )
    ("wd" "web development" )
    ("db" "database" )
    ("gui3" "graphical user interface" )
    ("oop3" "object oriented programing" )
    ("os3" "operating system" )
    ;; programing
    ("eq" "==" )
    ("r" "return" )
    ("utf8" "-*- coding: utf-8 -*-" )
    ;; regex
    ("xaz" "\\([A-Za-z0-9]+\\)" )
    ;; unicode
    ("md" "—" )
    ("hr" "--------------------------------------------------" )
    ("bu" "•" )
    ;; url
    ("urlemacs" "http://ergoemacs.org/" )))
(setq save-abbrevs nil)

;; define abbrev for specific major mode
;; the first part of the name should be the value of the variable major-mode of that mode
;; e.g. for go-mode, name should be go-mconcat
(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("for" "for i := 0; i < 4; i++ { i }")
    ("if" "if x < 0 { 3 }")
    ("r" "return")
    ("ps" "+")
    ("eq" "==")
    ("pt" "fmt.Println(3)")
    ("fu" "func(x int) int { return 1 }")
    ("v" "var = 3")))
;;(set-default 'abbrev-mode t)
;;(global-set-key (kbd "<tab>") 'expand-abbrev)

(set-default-font "-outline-DejaVu Sans Mono-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1")


;; not used temperarily
;; (use-package yari)
;; (use-package rinari
;;   :config
;;   (global-rinari-mode t))


;; unused key bindings
;; C-i is bound with TAB, so don't change this.
;; C-m is bound with RET, so don't change this.



;; org-mode configurations
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq gtd-directory (concat emacs-home "/GTD"))

(setq org-default-notes-file (expand-file-name "inbox.org" gtd-directory))

;; ! triggers a timestamp when states are changed
;; @ triggers a note when states are changed
(setq org-todo-keywords
      '((sequence "TODO(t!)" "PENDING(p!)" "|" "DONE(d!)" "CANCELED(c@/!)")))
(setq org-todo-keyword-faces
      '(("PENDING" . "orange")
 	("CANCELED" . "red")))
;; Switch entry to DONE when all subentries are done, to TODO otherwise.
(defun org-summary-todo (n-done n-not-done)
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-agenda-custom-commands
      '(("o" "At the office" tags-todo "@office")
	("h" "At home" tags-todo "@home")
	("w" "On the way" tags-todo "@way")))

(setq inbox-file (expand-file-name "inbox.org" gtd-directory))
(setq notes-file (expand-file-name "notes.org" gtd-directory))
(setq tasks-file (expand-file-name "tasks.org" gtd-directory))
(setq someday-file (expand-file-name "someday.org" gtd-directory))
(setq finished-file (expand-file-name "finished.org" gtd-directory))
(setq canceled-file (expand-file-name "canceled.org" gtd-directory))

;; I don't know why org-agenda-files must be a list of string literals here, variables cannot work.
(setq org-agenda-files '("D:\\emacs\\.emacs.d\\GTD\\inbox.org"
			 "D:\\emacs\\.emacs.d\\GTD\\tasks.org"
			 "D:\\emacs\\.emacs.d\\GTD\\someday.org"
			 "D:\\emacs\\.emacs.d\\GTD\\finished.org"
			 "D:\\emacs\\.emacs.d\\GTD\\canceled.org"))


(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline inbox-file "Inbox")
                               "* TODO %i%? %T %^g" :empty-lines-before 1)
                              ("n" "Notes" entry
                               (file+headline notes-file "Notes")
                               "* %i%? \n %T" :empty-lines-before 1 :prepend 1)))
(setq org-refile-targets '((inbox-file :maxlevel . 2)
                           (tasks-file :maxlevel . 2)
                           (someday-file :maxlevel . 2)
                           (finished-file :maxlevel . 2)
                           (canceled-file :maxlevel . 2)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rsense zzz-to-char zoutline zeno-theme youdao-dictionary yasnippet-snippets web-mode use-package typing solarized-theme smartparens scala-mode sbt-mode projectile overcast-theme nyan-mode neotree multiple-cursors meghanada magit js2-mode iedit hydra htmlize highlight-thing golden-ratio github-theme f expand-region emmet-mode eclipse-theme drag-stuff dash-functional counsel cider browse-kill-ring beacon all-the-icons-dired ahungry-theme ag ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
