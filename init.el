(setq gc-cons-threshold 100000000)
(let ((file-name-handler-alist nil))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(setq visible-bell t)       ; Set up the visible bell

;; MM Library
(load-file "/home/sebas/.emacs.d/mm-elisp/mm-latex-mode.el")
(load-file "/home/sebas/.emacs.d/mm-elisp/mm-windows.el")
(load-file "/home/sebas/.emacs.d/mm-elisp/mm-utils.el")

(defvar my-default-font-size 120)
;; Set font and it's size
(set-face-attribute 'default nil :font "Fira Code Retina" :height my-default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 160)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 190 :weight 'regular)

;; MM Elisp functions and keybinds
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
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
(global-set-key (kbd "C-M-l") 'right-word)
(global-set-key (kbd "C-m") 'back-to-indentation)
(global-set-key (kbd "RET") 'newline)
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

(global-set-key (kbd "C-n") 'electric-newline-and-maybe-indent)
(global-set-key (kbd "C-f") 'kill-line)
(global-set-key (kbd "C-p") 'help-command)
(global-set-key (kbd "C-b") 'recenter-top-bottom)
(global-set-key (kbd "C-M-o") 'counsel-switch-buffer)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c f") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)

(global-set-key (kbd "C-x K") 'mm/kill-everything)

(global-set-key (kbd "C-t") 'goto-line-preview)

(global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
(global-set-key (kbd "C-M-<up>") 'move-dup-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "M-RET") 'mm/split-window-horizontally-and-focus-vterm)
(global-set-key (kbd "C-x 2") 'mm/split-window-vertically-and-focus)
(global-set-key (kbd "C-x 3") 'mm/split-window-horizontally-and-focus)
(global-set-key (kbd "C-r") 'mm/go-to-saved-point)
(global-set-key (kbd "C-s") (lambda () (interactive) (mm/save-point-and-fn 'swiper)))
(global-set-key (kbd "C-M-s") 'mm/save-point)
(global-set-key (kbd "C-`") 'mm/toggle-vterm-below)

;; My personal keybind
(global-set-key (kbd "C-x C-x") 'yas-expand)

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

;; Entry screen
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-center-content t)
    (setq dashboard-banner-logo-title " Also try Vim.")
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-footer nil)
    (setq dashboard-startup-banner "~/.emacs.d/hatsumi.jpg"))
  :config
  (dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-items '(
                        (recents  . 4)
                        (projects . 3)
                        (agenda . 5)
                       ;;(bookmarks . 3)
                        )))

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

;; Better ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Bar at the bottom showing major and minor mode
  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 15)))

  ;; Doom modeline icons for emacsclient
  (setq doom-modeline-icon t)

  ;; Set Emacs theme
(use-package doom-themes
    :ensure t)
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)

(load-theme 'doom-moonlight t)

;; Better syntax highlighting
(use-package tree-sitter
  :ensure t)

;; Better tree sitter
(use-package tree-sitter-langs
  :defer
  :ensure t
  :config
  (tree-sitter-require 'tsx)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  (add-to-list 'tree-sitter-major-mode-language-alist '(jtsx-jsx-mode . tsx))
  (add-to-list 'tree-sitter-major-mode-language-alist '(jtsx-tsx-mode . tsx)))

;; Snippets of code (all 3 need to be installed with package-install RET package-name RET)
(use-package yasnippet
  :ensure t
  :defer
  :config
  (yas-global-mode)
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))



;; To add ts snippets jtsx modes create a .yas-parents file in snippets directory
;; in .emacs.d directory and write 'typescript-mode'
(use-package yatemplate)

;; Better file and buffer management
(use-package counsel
  :bind(("M-x" . counsel-M-x)
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

;; Better projectile mode
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Better Dired
(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode
         . all-the-icons-dired-mode))

(setf dired-kill-when-opening-new-dired-buffer t)

;; Single buffer for dired
(use-package dired-single
  :ensure t)

;; Completions and how to make them pretty
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
              ("<tab>" . company-indent-or-complete-common)
              )
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

;; Git porcelain
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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

;; Better terminal colors
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;; Increase the amount of data which Emacs reads from the process.
;; Default value is causing a slowdown, it's too low to handle server responses.
(setq read-process-output-max (*(* 1024 1024) 3)) ;; 3Mib
(setq lsp-headerline-breadcrumb-enable nil)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; restclient and resclient mode for .http files
(use-package restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;;emmet mode
  (use-package emmet-mode
    :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
(setq emmet-move-cursor-between-quotes t) ;; default nil
  (add-to-list 'emmet-jsx-major-modes 'jtsx-jsx-mode)
  (add-to-list 'emmet-jsx-major-modes 'jtsx-tsx-mode))

(with-eval-after-load "emmet-mode"
  (define-key emmet-mode-keymap (kbd "C-j") nil))

;; Enables lsp communcation
    (use-package lsp-mode
      :ensure t
      :commands (lsp lsp-deferred)
      :hook (lsp-mode . tree-sitter-mode)
      ;;:hook (lsp-mode . efs/lsp-mode-setup)
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

  ;;LSP mode for Typescript
  (use-package typescript-mode
    :mode "\\.[jt]s\\'"
    :config

    (setq typescript-indent-level 2)
    (add-hook 'typescript-mode-hook 'lsp-deferred)
    (add-hook 'typescript-mode-hook 'check-tsx)
    (add-hook 'typescript-mode-hook 'prettier-js-mode))

;; requires emmet mode to work correctly
(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx?\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode))
  :custom
  ;; Optional customizations
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  (jtsx-switch-indent-offset 0)
  ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
  ;; (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
  (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-jsx-element-tags-auto-sync nil)
  (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
  (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
  (define-key mode-map (kbd "C-c C-a") 'jtsx-jump-jsx-opening-tag)
  (define-key mode-map (kbd "C-c C-s") 'jtsx-jump-jsx-closing-tag)
  (define-key mode-map (kbd "C-c C-r") 'jtsx-rename-jsx-element)
  (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
  (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
  (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
  (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
  (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
  (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
  (define-key mode-map (kbd "C-c  C-w") 'jtsx-wrap-in-jsx-element)
  (define-key mode-map (kbd "C-c  C-u") 'jtsx-unwrap-jsx)
  (define-key mode-map (kbd "C-c  C-d") 'jtsx-delete-jsx-node))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map)
  (add-hook 'jtsx-jsx-mode-hook 'lsp)
  (add-hook 'jtsx-jsx-mode-hook 'tree-sitter-mode)
  (add-hook 'jtsx-tsx-mode-hook 'lsp)
  (add-hook 'jtsx-tsx-mode-hook 'tree-sitter-mode)
  (add-hook 'jtsx-jsx-mode-hook 'prettier-js-mode)
  (add-hook 'jtsx-tsx-mode-hook 'prettier-js-mode)
  (add-hook 'jtsx-jsx-mode-hook 'emmet-mode)
  (add-hook 'jtsx-tsx-mode-hook 'emmet-mode))

  (use-package quelpa
    :ensure t
    :config
    (use-package quelpa-use-package
      :ensure t))

  (use-package tsi
    :after tree-sitter
    ;; :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
    ;; define autoload definitions which when actually invoked will cause package to be loaded
    :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
    :init
    (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
    (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
    (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
    (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

  ;; LSP mode for HTML
  (use-package mhtml-mode
    :mode "\\.html\\'"
    :config
    (add-hook 'mhtml-mode-hook 'lsp))

  ;; LSP mode for CSS
  (use-package css-mode
    :mode "\\..?css\\'"
    :config
    (add-hook 'css-mode-hook 'lsp))

  ;; LSP mode for C/C++
  (add-hook 'c-mode-hook 'lsp)
  (setq-default c-basic-offset 4)
  (add-hook 'c++-mode-hook 'rebind)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'c++-mode-hook (lambda () (local-unset-key (kbd"C-M-h"))))
  (setq-default c++-basic-offset 4)

  (use-package clang-format
    :ensure t)
  (setq-default clang-format-fallback-style "WebKit")

  ;; LSP mode for Rust
  (use-package rustic
    :ensure t
    :hook (rustic-mode . lsp)
    :config
    (require 'lsp-rust)
    (setq lsp-rust-analyzer-completion-add-call-parenthesis t))

  ;; LSP mode for Emacs
  (add-hook 'emacs-lisp-mode-hook 'company-mode)

  ;; LSP mode for LaTex
  (add-hook 'LaTeX-mode-hook
            (local-set-key (kbd "C-c C-. M-c") 'mm/latex-compile)
            (local-set-key (kbd "C-c C-. M-v") 'mm/latex-compile-and-view)
            (lambda () (local-unset-key (kbd "C-j"))))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'tex-mode-hook 'lsp)

;; ORG mode configuration
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
   (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

;; list tags
(setq org-tag-alist
  '((:startgroup)
     ; Put mutually exclusive tags here
     (:endgroup)
     ("@errand" . ?E)
     ("@home" . ?H)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("planning" . ?p)
     ("publish" . ?P)
     ("batch" . ?b)
     ("note" . ?n)
     ("idea" . ?i)))

;; Moves done tasks to archive

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Use spaces as indentation
  (indent-tabs-mode nil)

  ;; Subword mode help with different nameing convenctions (camelCase, snake_case)
  (global-subword-mode 1)

  ;; Auto close (), "", {}
  (electric-pair-mode 1)
  (setq electric-pair-pairs
        '(
          (?\" . ?\")
          (?\{ . ?\})))

;; Refresh a file edited outside of emacs
(global-auto-revert-mode 1)

;;insert boilerplate when creating a file
(auto-insert-mode t)

  ;; Enabling column numbers
  (column-number-mode)
  (global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  treemacs-mode-hook
                  rustic-cargo-run-mode-hook
                  rustic-cargo-test-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  ;; Colorful parentheses when programming
  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  ;; truncates lines
  (setq-default truncate-lines t)

  ;; you can delete the selected text just by hitting the Backspace key ( 'DEL' )
  (delete-selection-mode 1)

  ;; NOTE: The first time you load your configuration on a new machine, you'll
  ;; need to run the following command interactively so that mode line icons
  ;; display correctly:
  ;;
  ;; M-x all-the-icons-install-fonts

  (use-package all-the-icons)

  ;; Multiple cursor in Emacs needs to be installed with M-x  package-install RET multiple-cursors RET
  (use-package multiple-cursors)

  ;; Shows where cursor is
  (use-package beacon
    :ensure t)
  (beacon-mode 1)

  ;; Better tex
  (use-package tex
    :ensure auctex
    :defer)

  ;; Better pdf
  (use-package pdf-tools
    :defer
    :ensure t)

  ;; Shortcut help
  (use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 1))

  ;; Alt + arrows for Emacs
  (use-package move-dup
    :ensure t)

  ;; Jump to desired line
  (use-package goto-line-preview
    :ensure t)

  (put 'downcase-region 'disabled nil)

;; Backup stored in backup folder
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

)
(setq gc-cons-threshold 800000)
