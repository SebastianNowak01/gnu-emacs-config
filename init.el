;; MM Library
(load-file "/home/sebas/.emacs.d/mm-elisp/mm-latex-mode.el")
(load-file "/home/sebas/.emacs.d/mm-elisp/mm-windows.el")
(load-file "/home/sebas/.emacs.d/mm-elisp/mm-utils.el")

(defvar my-default-font-size 120)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Set font and it's size
(set-face-attribute 'default nil :font "Fira Code Retina" :height my-default-font-size)

;; Set theme (init can fail so wombat can save eyes[dark mode])
(load-theme 'wombat)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; MM Elisp functions and keybinds
(global-set-key (kbd "M-RET") 'my-split-window-vertically-and-focus-vterm)
(global-set-key (kbd "C-x 2") 'my-split-window-vertically-and-focus)
(global-set-key (kbd "C-x 3") 'my-split-window-horizontally-and-focus)
(global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
(global-set-key (kbd "C-M-<up>") 'move-dup-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)
(global-set-key (kbd "C-t") 'goto-line-preview)
(global-set-key (kbd "C-h") 'backward-char)
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-M-h") 'left-word)
(global-unset-key (kbd "C-M-j"))
(global-set-key (kbd "C-M-j") nil)
(global-set-key (kbd "C-M-j") (lambda () (interactive) (next-line 4)))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (previous-line 4)))

(global-set-key (kbd "C-t") 'goto-line-preview)

;; Auto close (), "", {}
(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; These variables are responsible for doom theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(package-selected-packages
   '(yafolding yasnippet-snippets yasnippet lsp-java rustic clang-format tree-sitter-langs tree-sitter goto-line-preview move-dup dired-single flycheck lsp-ivy lsp-treemacs lsp-ui company-box typescript-mode dashboard lsp-mode magit counsel-projectile projectile all-the-icons helpful ivy-rich which-key rainbow-delimiters doom-themes counsel doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Refresh a file edited outside of emacs
(global-auto-revert-mode 1)

;; Enabling column numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		shell-mode-hook
		vterm-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Entry screen
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-center-content t)
    (setq dashboard-banner-logo-title "Also try Vim.")
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-footer nil)
    (setq dashboard-startup-banner "~/.emacs.d/xiao.png"))
  :config
  (dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-items '(
                        (recents  . 4)
                        (projects . 3)
                        ;;(agenda . 5)
                        (bookmarks . 3)
                        )))

;; truncates lines
(setq-default truncate-lines t)

;; you can delete the selected text just by hitting the Backspace key ( 'DEL' )
(delete-selection-mode 1)

(setq global-subword-mode t)

;; Searching through file made easier
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; No initial characters when opening search buffer
(setq ivy-initial-inputs-alist nil)

;; No parent and current directory will be shown in dired buffer
(setq ivy-extra-directories nil)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

;; Multiple cursor in Emacs needs to be installed with M-x  package-install RET multiple-cursors RET
(require 'multiple-cursors)

;; Shows where cursor is
(use-package beacon
  :ensure t)
(beacon-mode 1)

;; Bar at the bottom showing major and minor mode
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Set Emacs theme
(use-package doom-themes
  :init (load-theme 'doom-gruvbox))

;; Colorful parentheses when programming
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Shortcut help
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Better ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Better syntax highlighting
(use-package tree-sitter
  :ensure t)

;; Better tree sitter
(use-package tree-sitter-langs
  :defer
  :ensure t)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Snippets of code (all 3 need to be installed with package-install RET package-name RET)
(use-package yasnippet
  :ensure t
  :defer
  :config
  (yas-global-mode)
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))
(yafolding-mode 1)

;; Better tex
(use-package tex
  :ensure auctex
  :defer)

;; Better pdf
(use-package pdf-tools
  :defer
  :ensure t)

;; Single buffer for dired
(use-package dired-single
  :ensure t)

;; Alt + arrows for Emacs
(use-package move-dup
  :ensure t)

;; Jump to desired line
(use-package goto-line-preview
  :ensure t)

;; Better file and buffer management
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Better function and variable description
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Better project movement
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Better Dired
(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode
         . all-the-icons-dired-mode))

(setf dired-kill-when-opening-new-dired-buffer t)

;; Better projectile mode
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Git porcelain
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Backup stored in backup folder
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Terminal emulator in Emacs
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))

  ;; Fix broken prompt and completion prompts while running fish shell
(with-eval-after-load 'vterm(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))))

;; Enables lsp communcation
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; Helpful ui lsp tweaks
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Treemacs
(use-package lsp-treemacs
  :after lsp)
(treemacs-project-follow-mode t)

(use-package lsp-ivy)

;; Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;; Increase the amount of data which Emacs reads from the process.
;; Default value is causing a slowdown, it's too low to handle server responses.
(setq read-process-output-max (*(* 1024 1024) 3)) ;; 3Mib
(setq lsp-headerline-breadcrumb-enable nil)

;; Completions and how to make them pretty
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
  (setq company-tooltip-maximum-width 60)
  (setq company-tooltip-margin 3)

;; Prettier completions
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))
(setq company-box-doc-enable t)

;; LSP mode for Typescript and Javascript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :mode "\\.js\\'"
  :config
  (setq typescript-indent-level 2)
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook 'lsp))

;; LSP mode for C/C++
(add-hook 'c-mode-hook 'lsp)
(setq-default c-basic-offset 4)
(add-hook 'c++-mode-hook 'rebind)
(add-hook 'c++-mode-hook 'lsp)
(setq-default c++-basic-offset 4)

(use-package clang-format
  :ensure t)
(setq-default clang-format-fallback-style "WebKit")

;; LSP mode for Rust
(use-package rustic
  :ensure t
  :hook (rustic-mode . lsp-deferred)
  :config
  (require 'lsp-rust)
  (setq lsp-rust-analyzer-completion-add-call-parenthesis t))

;; LSP mode for Emacs
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; LSP mode for Java
(use-package lsp-java
  :hook java-mode-hook)
(add-hook 'java-mode-hook 'lsp)

;; LSP mode for LaTex
(setq TeX-auto-save t)
(setq TeX-parse-self t) 
(add-hook 'tex-mode-hook 'lsp)
