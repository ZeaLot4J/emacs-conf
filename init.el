;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("5f27195e3f4b85ac50c1e2fac080f0dd6535440891c54fcfa62cdcefedf56b1b" default)))
 '(eww-search-prefix "https://www.bing.com/search?q=")
 '(package-archives
   (quote
    (("melpa-stable" . "http://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("gnu-cn" . "http://elpa.emacs-china.org/gnu/")
     ("melpa-cn" . "http://elpa.emacs-china.org/melpa/"))))
 '(package-selected-packages
   (quote
    (smooth-scroll which-key iedit expand-region js2-mode web-mode smex smartparens monokai-theme company)))
 '(send-mail-function (quote mailclient-send-it))
 '(smooth-scroll-margin 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:inherit highlight :background "orange" :foreground "gray15")))))

;; Features
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'init-common)






