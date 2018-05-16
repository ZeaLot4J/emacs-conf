;; Author: Lancelot Zealot
;; Date: 2018-05-15 Tuesday 21:11:52 CST
;; Filename: Emacs configuration

;; emacs package sources
(package-initialize)
(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("gnu-cn" . "http://elpa.emacs-china.org/gnu/")
	("melpa-cn" . "http://elpa.emacs-china.org/melpa/")))

;; ensure packages that are not installed yet will be installed automatically
(setq use-package-always-ensure t)

;; ahungry theme
(use-package ahungry-theme
  :config
  (load-theme 'ahungry t))

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

;; dependency of counsel and swiper
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))
;; smooth scroll
(use-package smooth-scroll
  :config
  (smooth-scrolling-mode t)
  (setq smooth-scroll-margin 2))

;; multi-editing with C-;
(use-package iedit)
;; expand or contract selected region
(use-package expand-region
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))
;; js mode
(use-package js2-mode)
;; web mode
(use-package web-mode
  :config
  (add-hook 'web-mode-hook '(lambda () ((setq web-mode-markup-indent-offset 2)
					(setq web-mode-css-indent-offset 2)
					(setq web-mode-code-indent-offset 2)))))
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode))
       auto-mode-alist))


;; complete parentheses smartly, including () [] {} ""
(use-package smartparens
  :config
  (smartparens-global-mode t))
;; complete strings
(use-package company
  :config
  (global-company-mode t))

;; emacs git
(use-package magit
  :bind ("C-x g" . git-status))

;; convenient lisp mode
(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode))

;; list recently open files with C-x C-r
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

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
;; turn off cursor's blink
(blink-cursor-mode -1)
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
;; open this config file with F2
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
;; indent the while buffer
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
;; search a char until it is found, just like Vim's command f
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
		     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(define-key global-map (kbd "<f3>") 'wy-go-to-char)
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

;; set a macro to copy the current word
(fset 'copy-word-at-point
   [?\C-= ?\M-w])
(global-set-key (kbd "C-`") 'copy-word-at-point)

;; 目前还有
;; C-i
;; C-m
;; C-;
;; 没有用到
