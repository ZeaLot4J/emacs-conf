;; Author: Lancelot Zealot
;; Date: 2018-05-15 Tuesday 21:11:52 CST
;; File description: Emacs configuration

;; emacs package sources
(package-initialize)
(setq package-archives	       
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("gnu-cn" . "http://elpa.emacs-china.org/gnu/")
	("melpa-cn" . "http://elpa.emacs-china.org/melpa/")))


;; ensure packages that are not installed yet will be installed automatically
(setq use-package-always-ensure t)

(use-package ahungry-theme)

(if (display-graphic-p)
    (load-theme 'ahungry t) 		;t means no load theme confirm
  (load-theme 'tsdh-dark t)
  )


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
  :bind ("C-s" . swiper))

(use-package ag)

;; dependency of counsel and swiper
;; what's more, it makes switch-to-buffer display a list
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))


;; smooth scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode t)
  (setq smooth-scroll-margin 2))

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
  (add-hook 'nxml-mode-hook 'yas-minor-mode))
;;  (add-hook 'jdee-mode-hook 'yas-minor-mode)


(use-package yasnippet-snippets)

;; powerful multiple cursor operations instead of iedit
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
;;   (setq neo-theme 'arrow))

;;  :config
;;  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

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



;; (use-package jdee
;;   :config
;;   (setq jdee-jdk-registry
;; 	'(("1.8.0_161" . "C:\\Program Files\\Java\\jdk1.8.0_161\\bin")))
;;   (setq jdee-compiler (quote ("javac server"))))

;; jump to any char's position quickly and conveniently
(use-package avy
  :bind ("<f3>" . avy-goto-char))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (ace-window-display-mode t))

;; split windows with golden ratio
(use-package golden-ratio
  :bind ("<f12>" . golden-ratio))

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
;;(menu-bar-mode -1)
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
		   (find-file "~/.emacs.d/init.el")))
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
;; cache accessed urls
(setq url-automatic-caching t)
;; turn off error bell and blink
(setq visible-bell nil
      ring-bell-function 'ignore)
;; emacs's inner copying and cutting will be effective with OS's clipboard
(setq select-enable-clipboard t)

;; Deprecated
;; search a char until it is found, just like Vim's command f
;; (defun wy-go-to-char (n char)
;;   "Move forward to Nth occurence of CHAR.
;; Typing `wy-go-to-char-key' again will move forwad to the next Nth
;; occurence of CHAR."
;;   (interactive "p\ncGo to char: ")
;;   (search-forward (string char) nil nil n)
;;   (while (char-equal (read-char)
;; 		     char)
;;     (search-forward (string char) nil nil n))
;;   (setq unread-command-events (list last-input-event)))
;; (define-key global-map (kbd "<f3>") 'wy-go-to-char)


;; quickly jump to specified line
(global-set-key (kbd "<f2>") 'goto-line)
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
;; config emails, need .authinfo
(setq user-full-name "Lancelot Zealot")
(setq user-mail-address "18879538430@163.com")
(setq smtpmail-smtp-server "smtp.163.com")
(setq smtpmail-smtp-service 25)
(setq send-mail-function ''smtpmail-send-it)
;; eww's default search engine
(setq eww-search-prefix "https://www.bing.com/search?q=")


;; move up and down a line or a region conveniently with  M-up and  M-down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
	(forward-line)
	(when (or (< arg 0) (not (eobp)))
	  (transpose-lines arg))
	(forward-line -1))
      (move-to-column column t)))))
(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))
(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "<M-up>") 'move-text-up)

;; set a macro to copy the current word at the point
(fset 'copy-word-at-point
      [?\C-= ?\M-w])
(global-set-key (kbd "C-`") 'copy-word-at-point)

(fset 'newline-at-any-point
      [?\C-e return])
(global-set-key (kbd "C-o") 'newline-at-any-point)


(set-default-font "-outline-DejaVu Sans Mono-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1")

;; not used temperarily
;; (use-package yari)
;; (use-package rinari
;;   :config
;;   (global-rinari-mode t))


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
;; e.g. for go-mode, name should be go-mode-abbrev-table
(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("g3" "package main
import \"fmt\"
func main() {
        fmt.Println(\"3\")}")
    ("for" "for i := 0; i < 4; i++ { i }")
    ("if" "if x < 0 { 3 }")
    ("r" "return")
    ("ps" "+")
    ("eq" "==")
    ("pt" "fmt.Println(3)")
    ("fu" "func(x int) int { return 1 }")
    ("v" "var = 3")))

;;(set-default 'abbrev-mode t)
(global-set-key (kbd "<tab>") 'expand-abbrev)



;; org-mode configurations
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)


(setq org-directory "D:\\emacs\\.emacs.d\\GTD")
(setq org-remember-templates '(
			       ("Task" ?t "** TODO %? %t\n %i\n %a" "~/GTD/inbox.org" "Tasks")
			       ("Book" ?c "** %? %t\n %i\n %a" "~/GTD/inbox.org" "Book")
			       ("Calendar" ?c "** %? %t\n %i\n %a" "~/GTD/inbox.org" "Calender")
			       ("Project" ?p "** %? %t\n %i\n %a" "~/GTD/inbox.org" "Project")))
(setq org-default-notes-file (concat org-directory "/inbox.org"))



;; ! triggers a timestamp when states are changed
;; @ triggers a note when states are changed
(setq org-todo-keywords
      '((sequence "TODO(t!)" "PENDING(p!)" "|" "DONE(d!)" "CANCELED(c!)")
	(sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
	(type "Fred" "Sara" "Lucy" "|" "DONE")))
(setq org-todo-keyword-faces
      '(("PENDING" . "orange")
	("CANCELED" . "red")))
;; Switch entry to DONE when all subentries are done, to TODO otherwise.
(defun org-summary-todo (n-done n-not-done)
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


;; UNUSED key bindings
;; C-i is bound with TAB, so don't change this.
;; C-m is bound with RET, so don't change this.



