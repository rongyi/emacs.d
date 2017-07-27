;; init file
;;  _ __ _   _        ___ _ __ ___   __ _  ___ ___
;; | '__| | | |_____ / _ \ '_ ` _ \ / _` |/ __/ __|
;; | |  | |_| |_____|  __/ | | | | | (_| | (__\__ \
;; |_|   \__, |      \___|_| |_| |_|\__,_|\___|___/
;;       |___/

;; debugging
(setq message-log-max 100000)
;; create dir and add it to load path
(defconst ry/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

(defun ry/emacs-subdirectory (d)
  "expand emacs subdir under ~/.emacs.d"
  (expand-file-name d ry/emacs-directory))

(let* ((subdirs '("elisp" "backup" "snippets"))
       (fulldirs (mapcar 'ry/emacs-subdirectory subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Making directory: %s" dir)
      (make-directory dir))))

(add-to-list 'load-path (ry/emacs-subdirectory "elisp"))

;; package initialization
(require 'package)
(setq package-archives '(("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")
                         ("gnu" . "http://elpa.emacs-china.org/gnu/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package dash
  :ensure t)

;; RongYi settings
(require 'rongyi-defun)
;; in the begnning, all function are defined in rongyi-defun,
;; editing is the first separated 'module'
(require 'rongyi-editing)

(require 'rongyi-basic)

(use-package evil
  :init
  :ensure t
  :config
  (defmacro evil-map (state key action)
    "Short hand for defining evil key"
    (let ((map (intern (format "evil-%S-state-map" state))))
      `(define-key ,map (kbd ,key) ,action)))

  (setq evil-want-C-i-jump nil)
  (evil-map insert "M-." 'insert-pointer-access)
  (evil-map insert "C-c" '(lambda ()
                            (interactive)
                            (save-excursion
                              (evil-normal-state)
                              (when (fboundp 'company-abort)
                                (company-abort))
                              (when (buffer-file-name)
                                (save-buffer)))))
  (evil-map visual "C-c" 'evil-normal-state)
  (evil-map normal "C-e" 'evil-end-of-line)
  (evil-map insert "C-e" 'evil-end-of-line)
  (evil-map normal "C-a" 'smarter-move-beginning-of-line)
  (evil-map insert "C-a" 'smarter-move-beginning-of-line)
  (evil-map insert "C-k" 'kill-line)
  (evil-map normal "C-f" 'evil-scroll-page-down)
  (evil-map insert "C-f" 'evil-forward-char)
  (evil-map normal "C-b" 'evil-scroll-page-up)
  (evil-map insert "C-b" 'evil-backward-char)
  (evil-map normal "C-d" 'evil-delete-char)
  (evil-map insert "C-d" 'evil-delete-char)
  (evil-map normal "C-n" 'evil-next-line)
  (evil-map insert "C-n" 'evil-next-line)
  (evil-map normal "C-p" 'evil-previous-line)
  (evil-map insert "C-p" 'evil-previous-line)
  (evil-map normal "C-w" 'ry/kill-region-or-backward-word)
  (evil-map insert "C-w" 'ry/kill-region-or-backward-word)
  (evil-map visual "C-w" 'ry/kill-region-or-backward-word)
  (evil-map insert "C-o" 'ry/open-line-above)
  ;; dont quite visual mode
  (evil-map visual "<" #'(lambda ()
                           (interactive)
                           (evil-shift-left (region-beginning) (region-end))
                           (evil-normal-state)
                           (evil-visual-restore)))
  (evil-map visual ">" #'(lambda ()
                           (interactive)
                           (evil-shift-right (region-beginning) (region-end))
                           (evil-normal-state)
                           (evil-visual-restore)))

  ;; we dont want to learn emacs keymap for jumping around
  (evil-map normal "C-]" 'helm-gtags-find-tag-from-here)
  (evil-map normal "C-t" 'helm-gtags-pop-stack)
  ;; pain in the ass
  (evil-map normal "K" nil)
  (evil-map motion "K" nil)
  (evil-map normal "/" 'helm-swoop)

  ;; make j == gj, visual line
  (setq evil-cross-lines t)
  (setq evil-want-visual-char-semi-exclusive t)

  (setq evil-move-cursor-back nil)
  (setq evil-emacs-state-cursor '("SkyBlue2" box))
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
  (setq evil-visual-state-cursor '("gray" (hbar . 2)))
  (setq evil-insert-state-cursor '("chartreuse3" bar))
  (setq evil-replace-state-cursor '("chocolate" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  ;; in git commit message or org mode, we'll using evil when we needed
  (evil-set-initial-state 'text-mode 'emacs)
  (evil-set-initial-state 'anaconda-mode-view-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'lisp-mode 'emacs)
  ;; http://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1)
  :diminish evil-mode)

;; count search number
(use-package evil-anzu
  :ensure t)


;; avy
(use-package avy
  :ensure t
  :bind (("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j b" . avy-pop-mark)
         ("C-c j j" . avy-goto-char-2)
         ("C-c j c" . avy-goto-char))
  :config
  (set-face-attribute 'avy-lead-face nil :foreground "gold" :weight 'bold :background nil)
  (set-face-attribute 'avy-lead-face-0 nil :foreground "deep sky blue" :weight 'bold :background nil))

;; expand-region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; helm
(use-package helm
  :ensure t
  :init
  (setq helm-command-prefix-key "C-c h")
  :config
  (require 'helm-config)
  (require 'helm-misc)
  (require 'helm-locate)

  ;; from https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
  (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))
  (defvar helm-source-header-default-height (face-attribute 'helm-source-header :height) )

  (defun helm-toggle-header-line ()
    "Hide the `helm' header is there is only one source."
    (if (> (length helm-sources) 1)
        (set-face-attribute 'helm-source-header
                            nil
                            :foreground helm-source-header-default-foreground
                            :background helm-source-header-default-background
                            :box helm-source-header-default-box
                            :height helm-source-header-default-height)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground (face-attribute 'helm-selection :background)
                          :background (face-attribute 'helm-selection :background)
                          :box nil
                          :height 0.1)))
  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

  (setq helm-quick-update t)
  (setq helm-bookmark-show-location t)

  (setq helm-buffers-fuzzy-matching           t
        helm-completion-in-region-fuzzy-match t
        helm-file-cache-fuzzy-match           t
        helm-imenu-fuzzy-match                t
        helm-mode-fuzzy-match                 t
        helm-ff-skip-boring-files             t
        helm-locate-fuzzy-match               t
        helm-quick-update                     t
        helm-recentf-fuzzy-match              t
        helm-semantic-fuzzy-match             t)


  ;;(global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-split-window-default-side 'other)
  (setq helm-split-window-in-side-p nil)
  (setq helm-display-function 'helm-default-display-buffer)
  (setq helm-adaptive-history-file (expand-file-name
                                    "helm-adapative-history"
                                    user-emacs-directory))

  ;; helm navidation on hjkl
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-q") 'helm-keyboard-quit)

  :bind (("C-x f" . helm-for-files)))

(use-package helm-ag
  :ensure t
  :bind (("C-c s a" . helm-ag-project-root)
         ("C-c s A" . helm-ag)
         ("C-c s c" . helm-ag-this-file))
  :config
  (setq helm-ag-fuzzy-match t
        helm-ag-insert-at-point 'symbol
        helm-ag-edit-save t))

(use-package helm-flx
  :ensure t
  :after helm
  :config
  (helm-flx-mode 1))

;; projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode 1)
  :commands projectile-ag
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ido
        projectile-indexing-method 'native ; force the use of native indexing in operating systems other than Windows
        projectile-find-dir-includes-top-level t
        projectile-globally-ignored-file-suffixes '(".o" ".lo" ".a" ".pyc"))
  :diminish projectile-mode)

;; magit
(use-package magit
  :ensure t
  :config

  (defun ry/edit-gitignore ()
    (interactive)
    (split-window-sensibly (selected-window))
    (find-file (expand-file-name ".gitignore" (magit-toplevel))))

  (defun ry/magit-cursor-fix ()
    (beginning-of-buffer)
    (when (looking-at "#")
      (forward-line 2)))
  ;; from howardabrams, make magit status fullscreen
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (add-hook 'git-commit-mode-hook 'ry/magit-cursor-fix)

  (global-set-key (kbd "<f2>") 'magit-status)
  (global-set-key (kbd "C-M-g") 'magit-status)
  (setq magit-commit-arguments '("--verbose")
        magit-save-repository-buffers 'dontask
        ;; use ido to look for branches
        magit-completing-read-function 'magit-ido-completing-read
        magit-log-buffer-file-locked t
        magit-revision-show-gravatars nil
        ;; put unstage before untracked, this is copied from repo and put untrack at the end
        magit-status-sections-hook '(magit-insert-status-headers
                                     magit-insert-merge-log
                                     magit-insert-rebase-sequence
                                     magit-insert-am-sequence
                                     magit-insert-sequencer-sequence
                                     magit-insert-bisect-output
                                     magit-insert-bisect-rest
                                     magit-insert-bisect-log
                                     magit-insert-unstaged-changes
                                     magit-insert-staged-changes
                                     magit-insert-stashes
                                     magit-insert-unpulled-from-upstream
                                     magit-insert-unpulled-from-pushremote
                                     magit-insert-unpushed-to-upstream
                                     magit-insert-unpushed-to-pushremote
                                     magit-insert-untracked-files)))

;; nyan cat
(use-package nyan-mode
  :ensure t
  :config
  (setq nyan-wavy-trail t))
;; spaceline: spacemacs's modeline
(use-package spaceline-config
  :ensure spaceline
  :after nyan-mode
  :config
  (defun ry/compute-powerline-height ()
    "Return an adjusted powerline height."
    (let ((scale 1.1))
      (truncate (* scale (frame-char-height)))))
  (setq-default powerline-height (ry/compute-powerline-height))

  (setq powerline-default-separator 'slant)
  (nyan-mode)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq spaceline-window-numbers-unicode t)
  (spaceline-spacemacs-theme))


;; flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode t)
  (flycheck-pos-tip-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
  (setq flycheck-standard-error-navigation nil)

  ;; Custom fringe indicator
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b01111111)))

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)

  ;; flycheck errors on a tooltip (doesnt work on console)
  (when (display-graphic-p (selected-frame))
    (with-eval-after-load 'flycheck
      (custom-set-variables
       '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
  (setq flycheck-clang-language-standard "c++11")
  (setq flycheck-flake8-maximum-line-length 160))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

;; silver searcher
(use-package ag
  :ensure t
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t
        ag-ignore-list '("elpa" ".git" ".venv" "venv" "GTAGS" "GPATH" "GRTAGS")
        ag-project-root-function (lambda (d)
                                   (let ((default-directory d))
                                     (projectile-project-root)))))

;; color variable: highlights each source code identifier uniquely based on its name
(use-package color-identifiers-mode
  :ensure t
  :config
  (global-color-identifiers-mode)
  :diminish color-identifiers-mode)

;; rainbow delimeters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

;; smartparen
(use-package smartparens
  :ensure t
  :config
  (defun ry/split-and-new-line ()
    "Split a quoted string or s-expression and insert a new line with
auto-indent."
    (interactive)
    (sp-split-sexp 1)
    (sp-newline))
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (sp-use-smartparens-bindings)
  ;; Ubuntu's workspace moving key is <C-M-left> <C-M-right>
  ;; and we dont want to change that
  (define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-backward-barf-sexp)
  (setq sp-show-pair-delay 0.2
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil)

  :diminish smartparens-mode)

;; company

;; (use-package company-flx
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'company
;;     (company-flx-mode +1)))

(use-package company
  :ensure t
  :config

  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-statistics-mode)
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-show-numbers t
        company-semantic-insert-arguments nil
        company-transformers '(company-sort-by-occurrence)
        company-global-modes '(not term-mode gud-mode)
        company-dabbrev-downcase nil
        company-require-match 'never
        ;; for YCM like completion, not very well
        ;; company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
        ;;                     company-preview-frontend
        ;;                     company-echo-metadata-frontend)
        ;; company-auto-complete t
        )
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-dabbrev)

  ;; cancel company explicitly
  (define-key company-active-map (kbd "C-g") 'company-abort)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)

  :diminish company-mode " ⓐ")

(use-package company-statistics
  :ensure t
  :after company
  :diminish company-statistics-mode)

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode)
  (setq company-quickhelp-delay 1))


;; python auto complete
(use-package python-mode
  :defer t
  :ensure nil
  :config
  (setq
   python-shell-interpreter "python"
   python-shell-interpreter-args ""
   python-indent-offset 4)

  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (add-hook 'after-save-hook 'prelude-python-mode-set-encoding nil 'local))

(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :bind (:map anaconda-mode-map
              ("C-c C-j" . anaconda-mode-find-definitions))
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; indent guide have some bugs
;; (use-package indent-guide
;;   :ensure nil
;;   :defer t
;;   :init
;;   (add-hook 'python-mode-hook 'indent-guide-mode)
;;   (setq indent-guide-delay 0.3)
;;   :config
;;   ;; we only want this in Python mode
;;   (indent-guide-global-mode -1)
;;   :diminish indent-guide-mode)

;; js
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (setq company-tern-property-marker "")
  (setq company-tern-meta-as-single-line t)
  (setq js2-highlight-level 3
        js2-basic-offset 2
        js2-cleanup-whitespace t
        js2-enter-indents-newline t
        js2-indent-on-enter-key t
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-pretty-multiline-declarations t
        js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$")
        js2-idle-timer-delay 0.1
        js2-strict-trailing-comma-warning t)
  :diminish js2-mode "JS")

(use-package tern
  :ensure t
  :defer t
  :init (add-hook 'js2-mode-hook 'tern-mode)
  :config
  ;; Don't generate port files
  (add-to-list 'tern-command "--no-port-file" 'append))

(use-package company-tern
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package json-mode
  :ensure t)

;; org ui candy
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1))))

;; close unnessary buffer automaticly
(use-package popwin
  :ensure t
  :config
  (add-to-list 'popwin:special-display-config `"*ag search*")
  (add-to-list 'popwin:special-display-config `("*magit-*" :noselect t))
  (add-to-list 'popwin:special-display-config `"*Flycheck errors*")
  (add-to-list 'popwin:special-display-config `"*Occur*")
  (add-to-list 'popwin:special-display-config `("*Compile-Log*" :noselect t))
  (add-to-list 'popwin:special-display-config `("*Paradox Report*" :noselect t))
  (add-to-list 'popwin:special-display-config `("\\*godoc" :regexp t))
  (add-to-list 'popwin:special-display-config `("*Messages*" :noselect nil))
  (add-to-list 'popwin:special-display-config `("*Anaconda*" :noselect t :position bottom :height 20))
  (add-to-list 'popwin:special-display-config `("*Completions*" :noselect nil :position bottom :height 20))
  (add-to-list 'popwin:special-display-config `("*slime-macroexpansion*" :noselect nil :position bottom :height 20))
  (popwin-mode 1))


;; ethan-wspace: OCD about whitespace
(use-package ethan-wspace
  :ensure t
  :config
  (setq mode-require-final-newline nil
        require-final-newline nil)
  (global-ethan-wspace-mode 1)
  :diminish ethan-wspace-mode)

;; Enhance C-x o when more than two window are open
(use-package ace-window
  :ensure t
  :config
  ;; rarely use this key now, spaceline containing alt-1 like key to navigate between windows
  ;; (global-set-key (kbd "C-x C-o") 'ace-window)

  ;; it seems like we dont need swap window frequently
  (global-set-key (kbd "C-x o") 'ace-window)

  (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 2.0)
  (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
  :diminish ace-window-mode)

;; snippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-M-,") 'yas-expand)
  (add-to-list 'yas-snippet-dirs (ry/emacs-subdirectory "snippets"))
  :diminish (yas-minor-mode . " Ⓨ"))

;; undo-tree (use C-x u to visualize, C-_ to undo, M-_ to redo)
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

;; ycmd for emacs
(use-package ycmd
  :ensure t
  :config
  ;; cancel argument

  (dolist (hook '(c-mode-hook c++-mode-hook go-mode-hook cc-mode-hook))
    (add-hook hook #'ycmd-mode))
  (set-variable 'ycmd-server-command '("python" "/usr/local/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name ".emacs.d/ycm_extra_conf.py" (getenv "HOME")))
  ;; make it larger
  (setq ycmd-max-num-identifier-candidates 30
        ycmd-extra-conf-handler 'load
        ycmd-force-semantic-completion t)
  (require 'ycmd-eldoc)
  (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

(use-package company-ycmd
  :ensure t
  :config
  (company-ycmd-setup))

(use-package flycheck-ycmd
  :ensure t
  :config
  (flycheck-ycmd-setup))


;; golang config
(use-package go-mode
  :ensure t
  :after flycheck
  :config
  (defun ry/go-test(prefix)
    "a shortcut to run go demo when learning golang"
    (interactive "p")
    (let* ((file (buffer-file-name))
           (path (file-name-directory file))
           (output (file-name-nondirectory  (file-name-sans-extension file))))
      (ry/visit-term-buffer)
      (insert (format "cd %s && go build %s" path file))
      (comint-send-input)
      (cond
       ((> prefix 1) (other-window -1))
       (t (insert (format "./%s" output))
          (comint-send-input)))
      (message "current file builded.")))
  (defun ry/go-tab-less-evil ()
    (setq tab-width 4 ; not the same with C/C++, prefer go-fmt favor
          indent-tabs-mode t)
    ;; tabs are fine in go mode
    (setq ethan-wspace-errors
          (remove 'tabs ethan-wspace-errors)))
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook #'ry/go-tab-less-evil)

  (define-key go-mode-map (kbd "C-c C-f") 'gofmt)
  (define-key go-mode-map (kbd "C-c C-c") 'ry/go-test)
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  ;; the same key as show python function doc in anaconda mode
  (define-key go-mode-map (kbd "M-?") 'godoc-at-point)
  (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
  (define-key go-mode-map (kbd "M-=") (lambda ()
                                        (interactive)
                                        (insert ":=")))
  (define-key go-mode-map (kbd "M-<") (lambda ()
                                        (interactive)
                                        (insert "<-")))
  (setq godoc-at-point-function 'godoc-gogetdoc))

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; go auto complete, give ycmd a try
(use-package company-go
  :ensure t
  :config
  ;; (add-to-list 'company-backends 'company-go)
  )
;; golint
(use-package golint
  :ensure t)



(define-key shell-mode-map (kbd "C-n") 'comint-next-input)
(define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
(add-hook 'shell-mode-hook (lambda ()
                             (company-mode -1)
                             (yas-minor-mode -1)))


;; from lunaryorn
(use-package which-func                 ; Current function name
  :init (which-function-mode)
  :config
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")))
  (setq-default header-line-format '((which-func-mode ("" which-func-format " ")))))


(use-package beacon                     ; Highlight cursor position in buffer
  :ensure t
  :init (beacon-mode 1)
  :config
  (setq beacon-color "purple")
  ;; in shell, we always focus at the point
  (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
  :diminish beacon-mode)


;; Package manager and init file
(use-package paradox                    ; Better package menu
  :ensure t
  :config
  (setq paradox-execute-asynchronously nil ; No async update, please
        paradox-spinner-type 'moon      ; Fancy spinner
        ;; Show all possible counts
        paradox-display-download-count t
        paradox-display-star-count t
        ;; Hide download button, and wiki packages
        paradox-use-homepage-buttons nil ; Can type v instead
        paradox-hide-wiki-packages t))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

(use-package windmove                   ; Move between windows with Shift+Arrow, just like my tmux conf
  :ensure t
  :bind (("M-h"  . windmove-left)
         ("M-l" . windmove-right)
         ("M-k"    . windmove-up)
         ("M-j"  . windmove-down)))

(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))


(use-package golden-ratio               ; Automatically resize windows
  :ensure t
  :init
  (defun ry/toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c =" . ry/toggle-golden-ratio))
  :config
  (setq golden-ratio-extra-commands '(windmove-up
                                      windmove-down
                                      windmove-left
                                      windmove-right
                                      ace-window
                                      ace-delete-window
                                      ace-select-window
                                      ace-swap-window
                                      ace-maximize-window
                                      select-window-0
                                      select-window-1
                                      select-window-2
                                      select-window-3
                                      select-window-4
                                      select-window-5 ;5 will do
                                      )
        ;; Exclude a couple of special modes from golden ratio, namely
        ;; Flycheck's error list, calc
        golden-ratio-exclude-modes '(flycheck-error-list-mode
                                     calc-mode
                                     dired-mode
                                     gdb-locals-mode
                                     gdb-registers-mode
                                     gdb-breakpoints-mode
                                     gdb-threads-mode
                                     gdb-frames-mode
                                     gdb-inferior-io-mode
                                     ediff-mode
                                     )
        ;; Exclude a couple of special buffers from golden ratio, namely Helm,
        ;; WhichKey, NeoTree, etc.
        golden-ratio-exclude-buffer-regexp
        `(,(rx bos "*" (any "h" "H") "elm*" eos)
          ,(rx bos "*which-key*" eos)
          ,(rx bos "*NeoTree*" eos)))
  :diminish (golden-ratio-mode . "ⓖ"))

                                        ; Save buffers when focus is lost
(use-package focus-autosave-mode
  :ensure t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

                                        ; Highlight TODOs in buffers
(use-package hl-todo
  :ensure t
  :defer t
  :init (global-hl-todo-mode))


;; steal from github.com/howardabrams/dot-files.git
(use-package fancy-narrow
  :ensure t
  :config
  (defun ry/highlight-block ()
    "Highlights a 'block' in a buffer defined by the first blank
          line before and after the current cursor position. Uses the
          'fancy-narrow' mode to high-light the block."
    (interactive)
    (let (cur beg end)
      (setq cur (point))
      (setq end (or (re-search-forward  "^\s*$" nil t) (point-max)))
      (goto-char cur)
      (setq beg (or (re-search-backward "^\s*$" nil t) (point-min)))
      (fancy-narrow-to-region beg end)
      (goto-char cur)))

  (defun ry/highlight-section (num)
    "If some of the buffer is highlighted with the `fancy-narrow'
          mode, then un-highlight it by calling `fancy-widen'.

          If region is active, call `fancy-narrow-to-region'.

          If NUM is 0, highlight the current block (delimited by blank
          lines). If NUM is positive or negative, highlight that number
          of lines.  Otherwise, called `fancy-narrow-to-defun', to
          highlight current function."
    (interactive "p")
    (cond
     ((fancy-narrow-active-p)  (fancy-widen))
     ((region-active-p)        (fancy-narrow-to-region (region-beginning) (region-end)))
     ((= num 0)                (ha/highlight-block))
     ((= num 1)                (fancy-narrow-to-defun))
     (t                        (progn (ha/expand-region num)
                                      (fancy-narrow-to-region (region-beginning) (region-end))))))

  :bind ("C-M-+" . ry/highlight-section))


(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎"))
        )
  (which-key-declare-prefixes
    "C-c w" "windows/frames"
    "C-c j" "jump"
    "C-c f" "files/buffers"
    "C-c !" "flycheck"
    "C-c &" "yasnippet"
    "C-c /" "google-this"
    "C-c m" "visual bookmark"
    "C-c e" "editing"
    "C-c s" "searching"
    "C-c g" "git"
    "C-c C-d" "slime document")
  :diminish which-key-mode)

;; from joedicastro
(use-package dired
  :init
  ;; human-readable sizes
  (setq dired-listing-switches "-alh")
  ;; 'a' reuses the current buffer, 'RET' opens a new one
  (put 'dired-find-alternate-file 'disabled nil)

  ;; '^' reuses the current buffer
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "^")
                (lambda ()
                  (interactive)
                  (find-alternate-file ".."))))))

;; [[https://github.com/alpaker/Fill-Column-Indicator][fill-column-indicator]] toggle the vertical column that indicates the fill
;; threshold.

(use-package fill-column-indicator
  :ensure nil
  :commands fci-mode
  :config
  (fci-mode)
  (setq fci-rule-column 80))

;; start a server
(use-package server
  :ensure t
  :config
  (or (server-running-p) (server-start)))

;; slime company
(use-package slime-company
  :ensure t
  :init
  (slime-setup '(slime-fancy slime-company)))

;; slime for lisp
(use-package slime
  :ensure t
  :commands slime
  :init
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))
  (if (equal system-type 'darwin)
      (setq inferior-lisp-program "/usr/local/bin/clisp")
    (setq inferior-lisp-program "/usr/bin/sbcl"))
  (slime-setup '(slime-fancy slime-company))
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t
        slime-auto-start 'always
        common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/"))

;; ggtags for reading kernel code
(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1))))
  :diminish ggtags-mode)

;; rainbow mode to see color
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode)

(use-package helm-gtags
  :ensure t
  :init
  ;; from the document
  ;; Enable fuzzy match. You should set this value before loading helm-gtags.el
  (setq helm-gtags-fuzzy-match t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (eq system-type 'darwin)
    (setq exec-path-from-shell-variables '("PATH"  "MANPATH" "SHELL" "GOPATH" "GOROOT"))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH" "GOROOT" "GOPATH" "SHELL"))))


(use-package dumb-jump
  :ensure t
  :config
  ;; Vim habit
  (define-key dumb-jump-mode-map (kbd "C-M-g") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-p") nil)
  ;; similar to C-] C-[
  (global-set-key (kbd "C-}") 'dumb-jump-go)
  (global-set-key (kbd "C-{") 'dumb-jump-back)
  (dumb-jump-mode)
  :diminish dumb-jump-mode)

;; Are you a theme slut?

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-day t))

;; (use-package moe-theme
;;   :ensure t
;;   :config
;;   (moe-light))

;; (load-theme 'leuven t)
;; (use-package spacemacs-theme
;;   :ensure t
;;   :config
;;   (load-theme 'spacemacs-dark t))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; (load-theme 'solarized-dark t)
;; (use-package base16-theme
;;   :ensure t
;;   :config
;;   (load-theme 'base16-ashes t))

;; (load-theme 'spacemacs-dark t)

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.5)
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  (add-hook 'prog-mode-hook (lambda ()
                              (highlight-symbol-mode)
                              (highlight-symbol-nav-mode)))
  :diminish highlight-symbol-mode)

;; manual switch on/off
(use-package focus
  :ensure t)

;; just like swiper, but swiper has some bug in it
(use-package helm-swoop
  :ensure t
  :bind (("C-c s s" . helm-swoop)
         ("C-c s S" . helm-multi-swoop)
         ("C-c s C-s" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-use-fuzzy-match nil
        helm-swoop-split-window-function 'helm-default-display-buffer))

;; using in Python mode when needed
(use-package highlight-indentation
  :ensure t)

(use-package demo-it
  :ensure t)

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode 1)
  :diminish window-numbering-mode)

;; have some visual effect for copy & paste
(use-package volatile-highlights
  :ensure t
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  :diminish volatile-highlights-mode)

(use-package visual-regexp
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace))
  :ensure t)

(use-package iedit
  :ensure t
  :config
  ;; use tab to navigate
  ;; use M-; to iedit-toggle-selection
  ;; TODO: key is different with highlight-symbol(M-n M-p)
  (global-set-key (kbd "C-:") 'iedit-mode)
  ;; we want M-; means comment everywhere!
  (define-key iedit-mode-keymap (kbd "M-;") nil))

(use-package highlight-parentheses
  :ensure t
  :init
  (setq hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4"))
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  :diminish highlight-parentheses-mode)


(use-package highlight-numbers
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)))
(use-package highlight-thing
  :ensure t
  :config
  (highlight-thing-mode 1)
  :diminish highlight-thing-mode)


(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  ;; Ignore some additional directories and file extensions
  (dolist (name '(".cask"
                  ".vagrant"
                  ".ensime_cache" ".ensime"
                  ".stack-work"))
    ;; Ignore some additional directories
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (dolist (ext '(".fls" ".out" ; LaTeX
                 ))
    (add-to-list 'ignoramus-file-endings ext))

  (ignoramus-setup))

(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'" ; Package files
                              "/itsalltext/" ; It's all text temp files
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p))
  (recentf-mode 1))

(use-package stripe-buffer              ; Add stripes to a buffer
  :ensure t
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))


(use-package helm-descbinds             ; Describe key bindings with Helm
  :ensure t
  :init (helm-descbinds-mode))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :after ibuffer-vc
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename))))



(use-package desktop                    ; Save buffers, windows and frames
  :disabled t
  :config
  ;; Save desktops a minute after Emacs was idle.
  (setq desktop-auto-save-timeout 60)

  (dolist (mode '(magit-mode magit-log-mode))
    (add-to-list 'desktop-modes-not-to-save mode))
  (add-to-list 'desktop-files-not-to-save (rx bos "COMMIT_EDITMSG"))
  (desktop-save-mode))

(use-package helm-files                 ; Manage files with Helm
  :ensure helm
  :defer t
  :bind (("C-c f f" . helm-for-files)
         ("C-c f r" . helm-recentf))
  :config
  (setq helm-recentf-fuzzy-match t
        ;; Use recentf to manage file name history
        helm-ff-file-name-history-use-recentf t
        ;; Find libraries from `require', etc.
        helm-ff-search-library-in-sexp t)
  (when (eq system-type 'darwin)
    ;; Replace locate with spotlight for `helm-for-files'
    (setq helm-for-files-preferred-list
          (append (delq 'helm-source-locate
                        helm-for-files-preferred-list)
                  '(helm-source-mac-spotlight)))))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)

  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil))
  :diminish (auto-revert-mode . " Ⓐ"))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :init
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  ;; Refresh diff-hl after Magit operations
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))
;; (use-package comment-dwim-2
;;   :ensure t
;;   :bind (("M-;" . comment-dwim-2)))

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'modern-c++-font-lock-mode))

(use-package gdb-mi
  :ensure t
  :init
  (setq
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))

(use-package cc-mode
  :ensure t
  :config
  ;; The reason we don't use quickrun is most of the time
  ;; we will use gdb to debug the exec file, but quickrun
  ;; delete the exec file as default.
  (defun ry/cc-test(prefix)
    "test cc code"
    (interactive "p")
    (let* ((file (buffer-file-name))
           (path (file-name-directory file))
           (output (file-name-nondirectory  (file-name-sans-extension file))))
      (ry/visit-term-buffer)
      (insert (format "cd %s && g++ -g --std=c++11 %s -o %s"
                      path
                      (file-name-nondirectory  file)
                      output))
      (comint-send-input)
      ;; switch back to source code
      (cond
       ((> prefix 1) (other-window -1))
       (t (insert (format "./%s" output))
          (comint-send-input)))
      (message "current file builded.")))
  (define-key c++-mode-map (kbd "C-c C-c") 'ry/cc-test))


(use-package google-this
  :ensure t
  :config
  (google-this-mode 1)
  ;; quick search
  (define-key google-this-mode-map (kbd "C-c / /") 'google-this)
  :diminish google-this-mode)

;; mark with UI
(use-package bm
  :ensure t
  :bind (("C-c m m" . bm-toggle)
         ("C-c m n" . bm-next)
         ("C-c m p" . bm-previous)))

(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh"))

(use-package goto-chg
  :ensure t)

(use-package visual-fill-column
  :ensure nil
  :bind (("C-c w c" . visual-fill-column-mode))
  :defer t
  :config
  (setq-default visual-fill-column-center-text t
                visual-fill-column-width 150
                visual-fill-column-fringes-outside-margins nil))

(use-package etags
  :ensure t
  :config
  (global-set-key (kbd "M-*") 'pop-tag-mark))

(use-package protobuf-mode
  :ensure t)

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t
  :bind (("C-c e j" . json-reformat-region)))

;; Highlight Escape Sequences: https://github.com/dgutov/highlight-escape-sequences
(use-package highlight-escape-sequences
  :ensure t
  :diminish hes-mode)

(use-package adaptive-wrap
  :config
  (progn
    (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)))

;; haskell config
(use-package hindent
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :config
  (setq hindent-style "gibiansky"))

;; first update cabal, and then cabal install ghc-mod
;; using cabal mirror: https://mirrors.tuna.tsinghua.edu.cn/help/hackage/ if the download is slow
;; follow this link: https://www.haskell.org/downloads/linux
;; this will use the latest haskell ghc
(use-package ghc
  :ensure t
  :config
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package company-ghc
  :ensure t
  :config
  (add-to-list 'company-backends '(company-ghc :with company-dabbrev))
  (setq company-ghc-show-info t))

(use-package haskell-mode
  :ensure t
  :after hindent
  :config
  (add-hook 'haskell-mode-hook #'company-mode)
  (setq haskell-process-type 'ghci)
  (setq haskell-program-name "/usr/bin/ghci")
  (setq haskell-tags-on-save t
        haskell-process-log t
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)
  (add-hook 'haskell-mode-hook #'inf-haskell-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

  ;; http://futurismo.biz/archives/2662
  (defadvice inferior-haskell-load-file (after change-focus-after-load)
    "Change focus to GHCi window after C-c C-l command"
    (other-window 1))
  (ad-activate 'inferior-haskell-load-file)
  ;; shortcut for some unit input
  (define-key haskell-mode-map (kbd "M-=") (lambda ()
                                             (interactive)
                                             (insert "=>"))))

;; from howardabrams
(use-package lisp-mode
  :init
  (defconst lisp--prettify-symbols-alist
    '(("lambda"  . ?λ)      ; Shrink this
      ("."       . ?•)))    ; Enlarge this
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;; yaml mode
(use-package yaml-mode
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode)))

(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package ansible
  :ensure t
  :init (add-to-list 'auto-mode-alist
                     '("\\(group_vars/.+\\|host_vars/.+\\)" . yaml-mode)))

(use-package ansible-doc
  :ensure t)

(use-package bookmark
  :init (setq bookmark-save-flag 1)
  :config
  (defun ry/add-bookmark (name)
    (interactive
     (list (let* ((filename (file-name-base (buffer-file-name)))
                  (project (projectile-project-name))
                  (func-name (which-function))
                  (initial (format "%s::%s:%s " project filename func-name)))
             (read-string "Bookmark: " initial))))
    (bookmark-set name))
  :bind (("C-c b m" . ry/add-bookmark)
         ("C-c r m" . ry/add-bookmark)
         ("C-c r l" . helm-bookmarks)))


;; when everything is set, we make our evil leader bindings
(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-nvmap :prefix ","
                 "," 'goto-last-change
                 "l" 'linum-mode
                 "w" 'save-buffer
                 "q" 'kill-this-buffer
                 "c SPC" 'comment-or-uncomment-line-or-region
                 "b" 'helm-bookmarks
                 "f" 'avy-goto-char
                 "e" 'helm-semantic-or-imenu
                 "p" 'projectile-find-file
                 "g" 'magit-status
                 "s" 'helm-ag-project-root
                 "t" 'helm-gtags-select
                 "SPC" 'ethan-wspace-clean-all
                 "w" 'ace-window
                 "K" (lambda ()
                       (interactive)
                       (save-excursion
                         (other-window 1)
                         (quit-window)
                         (other-window 1)))
                 "i" 'ry/find-user-init-file
                 "r" 'helm-resume
                 "y" 'ry/copy-whole-buffer-to-clipboard
                 "?" 'helm-descbinds
                 "a" 'ido-switch-buffer))

;; diminish more minor mode
(diminish 'global-auto-revert-mode)
(diminish 'global-whitespace-mode)
(diminish 'subword-mode)
(diminish 'eldoc-mode)
(diminish 'subword-mode)
