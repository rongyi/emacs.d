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
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
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
;; in the begnning, all function are defined in rongyi-defun, it's time to make some change
(require 'rongyi-editing)

(require 'rongyi-basic)

(use-package evil
  :init
  (setq evil-want-C-i-jump nil)
  :ensure t
  :config

  (define-key evil-insert-state-map (kbd "M-.") 'insert-pointer-access)
  (define-key evil-insert-state-map (kbd "C-c") '(lambda ()
                                                   (interactive)
                                                   (save-excursion
                                                     (evil-normal-state)
                                                     (when (fboundp 'company-abort)
                                                       (company-abort))
                                                     )))
  (define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-page-down)
  (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-f") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-page-up)
  (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
  (define-key evil-visual-state-map (kbd "C-b") 'evil-backward-char)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-delete-char)
  (define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
  (define-key evil-visual-state-map (kbd "C-d") 'evil-delete-char)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-visual-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-visual-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-normal-state-map (kbd "C-w") 'evil-delete)
  (define-key evil-insert-state-map (kbd "C-w") 'evil-delete)
  (define-key evil-visual-state-map (kbd "C-w") 'evil-delete)
  (define-key evil-insert-state-map (kbd "C-o") 'ry/open-line-above)
  ;; we dont want to learn emacs keymap for jump
  (define-key evil-normal-state-map (kbd "C-]") 'helm-gtags-dwim)
  (define-key evil-normal-state-map (kbd "C-t") 'helm-gtags-pop-stack)
  ;; pain in the ass
  (define-key evil-normal-state-map (kbd "K") nil)

  ;; make j == gj, visual line
  (setq evil-cross-lines t)
  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-move-cursor-back nil)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
  (setq evil-visual-state-cursor '("gray" box))
  (setq evil-insert-state-cursor '("chartreuse3" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  ;; int git commit message or org mode, we'll using evil when we needed
  (evil-set-initial-state 'text-mode 'emacs)
  (evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'anaconda-mode-view-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  ;; http://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1)

  :diminish evil-mode)

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
  :config
  (require 'helm-config)
  (require 'helm-misc)
  (require 'helm-locate)

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


;; projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode 1)
  :commands projectile-ag
  :config
  (setq projectile-completion-system 'ido)
  (setq projectile-indexing-method 'native) ; force the use of native indexing in operating systems other than Windows
  (setq projectile-enable-caching t)
  :diminish projectile-mode)

;; magit
(use-package magit
  :ensure t
  :config

  (defun ry/edit-gitignore ()
    (interactive)
    (split-window-sensibly (selected-window))
    (find-file (expand-file-name ".gitignore" (magit-toplevel))))

  (global-set-key (kbd "<f2>") 'magit-status)
  (global-set-key (kbd "C-M-g") 'magit-status)
  (setq magit-commit-arguments '("--verbose")))

;; nyan cat
(use-package nyan-mode
  :ensure t)
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

  (setq powerline-default-separator 'wave)
  (nyan-mode)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq spaceline-window-numbers-unicode t)
  (spaceline-spacemacs-theme))


;; flycheck
(use-package flycheck-pos-tip
  :ensure t)
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
  (setq flycheck-clang-language-standard "c++11"))


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
  ;; ubuntu's workspace key is <C-M-left> <C-M-right>
  ;; and we dont want to change that
  (define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-backward-barf-sexp)
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
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
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

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode)
  (setq
   python-shell-interpreter "python"
   python-shell-interpreter-args "")
  ;; (define-key python-mode-map (kbd "C-c f") 'ry/format-python)
  :diminish anaconda-mode)

(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )

;; js
(use-package company-tern
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (add-to-list 'company-backends 'company-tern)
  (setq company-tern-property-marker "")
  (setq company-tern-meta-as-single-line t)
  (setq js2-highlight-level 3
        js2-basic-offset 2
        js2-pretty-multiline-declarations t)
  :diminish js2-mode "JS")

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
  ;; the key "combo" is fast than the least used C-x o
  ;; so we decide to make a change with swap hot key
  (global-set-key (kbd "C-x C-o") 'ace-window)
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
  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
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

  (add-hook 'c++-mode-hook 'ycmd-mode)
  ;; (add-hook 'python-mode-hook 'ycmd-mode)
  (set-variable 'ycmd-server-command '("python" "/home/ry/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd"))
  (set-variable 'ycmd-global-config "/home/ry/.emacs.d/ycm_extra_conf.py")
  ;; (set-variable 'ycmd-extra-conf-whitelist '(""))
  (require 'ycmd-eldoc)
  (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
  )

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
  :config
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  (define-key go-mode-map (kbd "C-c C-f") 'gofmt)
  (defun ry/go-test(prefix)
    "a shortcut to run go demo when learning golang"
    (interactive "p")
    (let* ((file (buffer-file-name))
           (path (file-name-directory file)))
      (visit-term-buffer)
      (insert (format "cd %s && go build %s" path file))
      (comint-send-input)
      (when (> prefix 1)
        (other-window -1))
      (message "current file builded.")))

  (define-key go-mode-map (kbd "C-c C-c") 'ry/go-test)
  (setq godoc-at-point-function 'godoc-gogetdoc))

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; go auto complete
(use-package company-go
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go)
  ;; the same key as show python function doc in anaconda mode
  (define-key go-mode-map (kbd "M-?") 'godoc-at-point)
  (define-key go-mode-map (kbd "M-=") (lambda ()
                                        (interactive)
                                        (insert ":=")))
  (define-key go-mode-map (kbd "M-<") (lambda ()
                                        (interactive)
                                        (insert "<-"))))
;; goline
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
  (setq beacon-color "red")
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
  (defun rongyi-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c =" . rongyi-toggle-golden-ratio))
  :config
  (setq golden-ratio-extra-commands '(windmove-up
                                      windmove-down
                                      windmove-left
                                      windmove-right
                                      ace-window
                                      ace-delete-window
                                      ace-select-window
                                      ace-swap-window
                                      ace-maximize-window)
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
    "C-c s" "searching")
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
  :ensure t)
;; slime for lisp
(use-package slime
  :ensure t
  :commands slime
  :init
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))
  (if (equal system-type 'darwin)
      (setq inferior-lisp-program "/usr/local/bin/clisp")
    (setq inferior-lisp-program "/usr/bin/clisp"))
  (slime-setup '(slime-fancy slime-company)))

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
  :ensure t)

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
  (global-set-key (kbd "C-}") 'dumb-jump-go)
  (global-set-key (kbd "C-{") 'dumb-jump-back)
  (dumb-jump-mode)
  :diminish dumb-jump-mode)

;; themes
;; (use-package color-theme-sanityinc-solarized
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-solarized-dark t))
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-eighties t))
(use-package moe-theme
  :ensure t
  :config
  (moe-dark))
;; (load-theme 'leuven t)

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
  (global-set-key (kbd "C-:") 'iedit-mode))

(use-package highlight-parentheses
  :ensure t
  :config
  (highlight-parentheses-mode)
  :diminish highlight-paren-mode)


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
  :defer t
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
  :defer t
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
      (visit-term-buffer)
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

(use-package goto-chg
  :ensure t)

;; when everything is set, we make our evil leader bindings
(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-nmap :prefix ","
                "," 'goto-last-change               ; normaly its ``m' , this is a workaround for my new keyboard
                "l" 'linum-mode
                "w" 'save-buffer
                "q" 'kill-this-buffer
                "c SPC" 'comment-or-uncomment-line-or-region
                "b" 'bookmark-bmenu-list
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
                "i" 'find-user-init-file))

;; diminish more minor mode
(diminish 'global-auto-revert-mode)
(diminish 'global-whitespace-mode)
(diminish 'subword-mode)
(diminish 'eldoc-mode)
(diminish 'subword-mode)
