;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ; 设定源码加载路径
;; (require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


(electric-pair-mode t)                       ; 自动补全括号
(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
(column-number-mode t)                       ; 在 Mode line 上显示列号
(global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t)                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(setq inhibit-startup-message t)             ; 关闭启动 Emacs 时的欢迎界面
(setq make-backup-files nil)                 ; 关闭文件自动备份
(add-hook 'prog-mode-hook #'hs-minor-mode)   ; 编程模式下，可以折叠代码块
(global-display-line-numbers-mode 1)         ; 在 Window 显示行号
(tool-bar-mode -1)                           ; 关闭 Tool bar
(when (display-graphic-p) (toggle-scroll-bar -1)) ; 图形界面时关闭滚动条


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; set shortkey	
(global-set-key (kbd "RET") 'newline-and-indent) ; 回车另起一行并缩进
(global-set-key (kbd "M-p") 'previous-line)		; 光标向上移动
(global-set-key (kbd "M-n") 'next-line)			; 光标向下移动
(global-set-key (kbd "M-b") 'backward-char)		; 光标向左移动
(global-set-key (kbd "M-f") 'forward-char)		; 光标向右移动
(global-set-key (kbd "M-a") 'move-beginning-of-line)	; 光标移至行首
(global-set-key (kbd "M-e") 'move-end-of-line)			; 光标移至行尾
(global-set-key (kbd "M-;") 'forward-word) 		; 向后移动一个词
(global-set-key (kbd "M-'") 'backward-word) 	; 向左移动一个词
(global-set-key (kbd "M-c") 'kill-ring-save)    ; M-c 为复制
(global-set-key (kbd "M-v") 'yank)				; 插入已移除文本 -- 黏贴
(global-set-key (kbd "M-,") 'pop-global-mark)	; 跳转到上一标记
;(global-set-key (kbd "<tab>") 'set-mark-command)
(global-set-key (kbd "M-/") 'hippie-expand)		; 代码文本补全
(global-set-key (kbd "M-w") 'scroll-down-command)         ; pageup
(global-set-key (kbd "M-s") 'scroll-up-command)       ; pagedowm
(global-set-key (kbd "M-k") 'kill-region)               ; cut region
(global-set-key (kbd "C-M-p") 'scroll-other-window)   ; let other window up
(global-set-key (kbd "C-M-n") 'scroll-other-window-down) ; let other window pagedowm
(global-set-key (kbd "M-l") 'backward-kill-word)   ;; remove custor left word
;; decide replace in the future
(global-set-key (kbd "M-r") 'delete-char)    ;; remove custor right word
		

(global-set-key (kbd "C-j") nil)
;; 删去光标所在行（在图形界面时可以用 "C-S-<DEL>"，终端常会拦截这个按法)
(global-set-key (kbd "C-y") 'kill-whole-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(atom-one-dark))
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "77fc61aea21dbce6db8cdea21e0d8e430cc576dd0b3383eb6ba3fa818e58613e" default))
 '(package-selected-packages
   '(doom-themes yasnippet-snippets use-package-hydra undo-tree treemacs-projectile rainbow-delimiters mwim multiple-cursors marginalia magit lsp-ui lsp-ivy google-this good-scroll flycheck dracula-theme dashboard dap-mode counsel-projectile company-box color-theme-sanityinc-tomorrow atom-one-dark-theme amx afternoon-theme)))

(require 'color-theme-sanityinc-tomorrow)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "google" :family "JetBrains Mono")))))



(eval-when-compile
  (require 'use-package))

;; 把一组特定场景的命令组织到一起,通过简单按键来进行调用
(use-package hydra
  :ensure t)

(use-package use-package-hydra
  :ensure t
  :after hydra)

;; counsel 插件
(use-package counsel
  :ensure t)
  
;; ivy 插件
(use-package ivy
  :ensure t                          ; 确认安装，如果没有安装过 ivy 就自动安装    
  :config                            ; 在加载插件后执行一些命令
  (ivy-mode 1)                       ; 加载后启动 ivy-mode
  (setq ivy-use-virtual-buffers t)   ; 一些官网提供的固定配置
  (setq ivy-count-format "(%d/%d) ") 
  :bind                              ; 以下为绑定快捷键
  ("C-s" . 'swiper-isearch)          
  ; 绑定快捷键 C-s 为 swiper-search，替换原本的搜索功能
  ("M-x" . 'counsel-M-x)             ; 使用 counsel 替换命令输入，给予更多提示
  ("C-x C-f" . 'counsel-find-file)   ; 使用 counsel 做文件打开操作，给予更多提示
  ("M-y" . 'counsel-yank-pop)        ; 使用 counsel 做历史剪贴板粘贴，可以展示历史
  ("C-x b" . 'ivy-switch-buffer)     ; 使用 ivy 做 buffer 切换，给予更多提示
  ("C-c v" . 'ivy-push-view)         ; 记录当前 buffer 的信息
  ("C-c s" . 'ivy-switch-view)       ; 切换到记录过的 buffer 位置
  ("C-c V" . 'ivy-pop-view)          ; 移除 buffer 记录
  ("C-x C-SPC" . 'counsel-mark-ring) ; 使用 counsel 记录 mark 的位置
  ("<f1> f" . 'counsel-describe-function)
  ("<f1> v" . 'counsel-describe-variable)
  ("<f1> i" . 'counsel-info-lookup-symbol))
  
;; foo插件 
(use-package foo
  :init                  ; 在加载插件前执行一些命令
  (setq foo-variable t)
  :config                ; 在加载插件后执行一些命令
  (foo-mode 1))
 
;; 语法检查插件 flycheck
(use-package flycheck
  :ensure t
  :hook                        ; 为模式设置 hook
  (prog-mode . flycheck-mode))
  
;; 记录短期 M-x 命令历史插件 amx
(use-package amx
  :ensure t
  :init (amx-mode))
  
;; 多窗口切换插件 ace-window
(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))
 
;; 重做M-a(按一次移至句首，按两次移至行首),M-e类似
(use-package mwim
  :ensure t
  :bind
  ("M-a" . mwim-beginning-of-code-or-line)
  ("M-e" . mwim-end-of-code-or-line))
  
;; undo-tree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :after hydra
  :bind ("C-x C-h u" . hydra-undo-tree/body)
  :hydra (hydra-undo-tree (:hint nil)
  "
  _p_: undo  _n_: redo _s_: save _l_: load   "
  ("p"   undo-tree-undo)
  ("n"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("u"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue)))
  
;; 美化model line
;(add-to-list 'custom-theme-load-path (expand-file-name "themes"
;                                                      user-emacs-directory))
;(use-package smart-mode-line
;  :config
;  (setq sml/theme 'atom-one-dark)
;  (sml/setup))
  
;; 美化滚动条
(use-package good-scroll
  :ensure t
  :if window-system          ; 在图形化界面时才使用这个插件
  :init (good-scroll-mode))

;; 快捷键提示
;(use-package which-key
;  :ensure t
;  :init (which-key-mode))
  
;; 文本快捷操作插件： avy
(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer)))

;; 为 Emacs minibuffer 中的选项添加注解的插件
(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
			  ("C-c n" . marginalia-cycle)))


;; 修改欢迎界面
 (use-package dashboard
  :ensure t
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-banner-logo-title "Welcome Vikingar!") ;; 个性签名，随读者喜好设置
  (setq dashboard-projects-backend 'projectile) ;; 读者可以暂时注释掉这一行，等安装了 projectile 后再使用
  (setq dashboard-startup-banner "/home/vikingar/Pictures/emacs/6.png") ;; 也可以自定义图片
  (setq dashboard-items '((recents  . 5)   ;; 显示多少个最近文件
			  (bookmarks . 5)  ;; 显示多少个最近书签
			  (projects . 10))) ;; 显示多少个最近项目
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (dashboard-setup-startup-hook))
 
;; 高亮括号
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


;; version management
(use-package magit
  :ensure t)


(provide 'init)

;;; init.el ends here

