;; 显示行号
(global-linum-mode t)
;; 关闭工具条
(tool-bar-mode -1)
;; 关闭滚动条
(scroll-bar-mode -1)
;; 关闭菜单
(menu-bar-mode -1)
;; 当打开emacs时，不显示欢迎界面
(setq inhibit-splash-screen t)
;; 按F2打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-common.el"))
(global-set-key (kbd "<f2>") 'open-init-file)
;; 开启补全模式
(global-company-mode t)
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


;; 括号智能补全，包括() {} [] ""等
(require 'smartparens-config)
(smartparens-global-mode t)

;; 高亮光标所在行
(global-hl-line-mode t)

;; 查询函数，变量，和按键所在的文件定义
(global-set-key "\C-h\ \C-f" 'find-function)
(global-set-key "\C-h\ \C-v" 'find-variable)
(global-set-key "\C-h\ \C-k" 'find-function-on-key)

;; 增强m-x的功能，能够自动提示命令
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; 原来的M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; 搜索，打开文件，查找文件，切换buffer，查看函数和变量时，会出现可选列表
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-x C-l") 'counsel-locate)


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

;; web文件的模式
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode))
       auto-mode-alist))
;; html, css, js缩进2
(defun my-web-mode-indent-setup ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-indent-setup)

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

;; 选中单词及扩大缩小区域
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; 批量编辑
(require 'iedit)
;; org模式下开启代码高亮
(setq org-src-fontify-natively t)

;; C-方向键，在多个窗口间移动光标
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)
(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-right>") 'windmove-right)

;; which key模式，打开快捷键提示
;(which-key-mode t)
;(setq which-key-side-window-location 'right)

;; 复制到系统剪切板和从剪切板粘贴
(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-S-v") 'clipboard-yank)

;; 有道词典
(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+)
;; 缓存URL
(setq url-automatic-caching t)

(provide 'init-common)
