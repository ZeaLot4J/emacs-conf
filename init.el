;; Author: Lancelot Zealot
;; Date: 2018年 05月 15日 星期二 21:11:52 CST
;; Filename: Emacs configuration

;; 设置包的下载源
(package-initialize)
(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("gnu-cn" . "http://elpa.emacs-china.org/gnu/")
	("melpa-cn" . "http://elpa.emacs-china.org/melpa/")))

;; 确保当包未安装时会自动安装
(setq use-package-always-ensure t)


;; ahungry主题
(use-package ahungry-theme
  :config
  (load-theme 'ahungry t))

;; 有道划词翻译
(use-package youdao-dictionary
  :bind ("C-c y" . youdao-dictionary-search-at-point+))

;; 开启在minibuffer中继续打开minibuffer的功能
(setq enable-recursive-minibuffers t)
;; 打开文件，M-x，描述函数，描述变量和搜索文件时，会出现可选择列表
(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
	 ("M-x" . counsel-M-x)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)
	 ("C-x C-l" . counsel-locate)))


;; 搜索字符串时，会出现可选择列表
(use-package swiper
  :bind ("C-s" . swiper))

;; counsel 和 swiper的依赖
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))
;; 平滑滚动 
(use-package smooth-scroll
  :config
  (smooth-scrolling-mode t)
  (setq smooth-scroll-margin 2))

;; 多光标编辑 C-;
(use-package iedit)
;; 扩大或缩小选中区域
(use-package expand-region
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))
;; js模式
(use-package js2-mode)
;; web模式
(use-package web-mode
  :config
  (add-hook 'web-mode-hook '(lambda () ((setq web-mode-markup-indent-offset 2)
					(setq web-mode-css-indent-offset 2)
					(setq web-mode-code-indent-offset 2)))))
;; web文件的模式
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode))
       auto-mode-alist))


;; 括号智能补全 () [] {} ""
(use-package smartparens
  :config
  (smartparens-global-mode t))
;; 字符串补全
(use-package company
  :config
  (global-company-mode t))


;; 背景透明
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist 'alpha '(85 . 50))

;; 显示行号
(global-linum-mode t)
;; 显示列号 (line number, column number)
(setq column-number-mode t)
;; 关闭工具条
(tool-bar-mode -1)
;; 关闭滚动条
(scroll-bar-mode -1)
;; 关闭菜单
(menu-bar-mode -1)
;; 当打开emacs时，不显示欢迎界面
(setq inhibit-splash-screen t)
;; 按F2打开配置文件
(global-set-key (kbd "<f1>")
		'(lambda ()
		   (interactive)
		   (find-file "~/.emacs.d/init.el")))
;; 禁止备份文件
(setq make-backup-files nil)
;; 禁止自动保存
(setq auto-save-default nil)
;; 按下C-x C-r 列出最近打开文件
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;; 选中文本时，按下任意键，会删除原文本，而不是在尾部追加
(delete-selection-mode t)
;; 全屏打开
(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; 高亮匹配的括号
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; 高亮距光标最近的配对括号
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))


;; 高亮光标所在行
(global-hl-line-mode t)

;; 查询函数，变量，和按键所在的文件定义
(global-set-key "\C-h\ \C-f" 'find-function)
(global-set-key "\C-h\ \C-v" 'find-variable)
(global-set-key "\C-h\ \C-k" 'find-function-on-key)

;; 全文件格式化
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

;; 用y or n代替yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; dired模式下删除和拷贝目录时，总是递归执行
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; 在dired模式下，访问文件永远只有一个buffer
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))




;; occur模式下直接对选择当前选中的字符进行查找
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
;; 显示当前文件的函数列表
(global-set-key (kbd "M-s i") 'counsel-imenu)

;; org模式下开启代码高亮
(setq org-src-fontify-natively t)

;; C-方向键，在多个窗口间移动光标
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)
(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-right>") 'windmove-right)


;; 缓存URL
(setq url-automatic-caching t)
;; 关闭错误提示音和闪烁
(setq visible-bell nil
      ring-bell-function 'ignore)
;; emacs内部的复制和粘贴对系统剪切板生效
(setq select-enable-clipboard t)
;; minibuffer处显示时间
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)


;; 类似于vim的f命令
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

;; 快速跳转到指定行
(global-set-key (kbd "<f2>") 'goto-line)
;; 开始录制宏
(global-set-key (kbd "<f7>") 'kmacro-start-macro-or-insert-counter)
;; 结束录制宏或者执行宏
(global-set-key (kbd "<f8>") 'kmacro-end-or-call-macro)
;; 关闭当前buffer以及窗口
(global-set-key (kbd "<f4>") 'kill-buffer-and-window)


;; 复制当前行
(global-set-key "\C-j"
		(lambda ()
		  (interactive)
		  (progn
		    (kill-whole-line)
		    (yank)
		    (message "copied line"))))
;; 删除当前行
(global-set-key (kbd "C-k") 'kill-whole-line)
;; 替换
(global-set-key (kbd "C-r") 'query-replace)


;; 一行超过80列则换行，配合M-q和auto-fill-mode使用
(setq default-fill-column 80)

;; 光标停止闪烁
(blink-cursor-mode -1)



;; 退出emacs时保存桌面 
(desktop-save-mode t)


;; 标记一个保存点，供后续操作之后跳转回来，类似vim的m
(global-set-key (kbd "C-,")
		'(lambda ()
		   (interactive)
		   (point-to-register ?\`))) ;使用｀寄存器
;; 跳转回之前标记的保存点
(global-set-key (kbd "C-.")
		'(lambda ()
		   (interactive)
		   (jump-to-register ?\`)))

;; 邮件配置,需要.authinfo
(setq user-full-name "Lancelot Zealot")
(setq user-mail-address "18879538430@163.com")
(setq smtpmail-smtp-server "smtp.163.com")
(setq smtpmail-smtp-service 25)
(setq send-mail-function (quote smtpmail-send-it))

;; eww浏览器默认搜索引擎
(setq eww-search-prefix "https://www.bing.com/search?q=")


;; 目前还有
;; C-i
;; C-m
;; C-;
;; 没有用到

