;; init file

;; debugging
(setq message-log-max 100000)
;; create dir and add it to load path
(defconst ry/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun ry/emacs-subdirectory (d)
  (expand-file-name d ry/emacs-directory))

(let* ((subdirs '("elisp" "backup"))
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

;; RongYi settings
(require 'rongyi-defun)

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
  (define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)
  (define-key evil-normal-state-map (kbd "C-t") 'pop-tag-mark)

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
  (evil-mode 1)

  :diminish evil-mode)

(use-package evil-anzu
  :ensure t)



;; avy
(use-package avy
  :ensure t)

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
                                    user-emacs-directory)))


;; projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode 1)
  :commands projectile-ag
  :config
  (setq projectile-completion-system 'ido)
  (setq projectile-indexing-method 'native) ; force the use of native indexing in operating systems other than Windows
  :diminish projectile-mode)

;; magit

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "<f2>") 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq magit-commit-arguments '("--verbose")))

;; nyan cat
(use-package nyan-mode
  :ensure t)
;; spaceline: spacemacs's modeline
(use-package spaceline-config
  :ensure spaceline
  :after nyan-mode
  :config
  (nyan-mode)
  (spaceline-spacemacs-theme))


;; powerline, deprecated!
;; (require-install-nessary 'powerline)
;; (setq powerline-default-separator 'wave)
;; (powerline-center-evil-theme)


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
       '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))))


;; silver searcher
(use-package ag
  :ensure t
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t
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
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :diminish smartparens-mode)

;; company

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-statistics-mode)
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-require-match nil
        company-show-numbers t
        company-transformers '(company-sort-by-occurrence)
        company-global-modes '(not term-mode))

  ;; cancel company explicitly
  (define-key company-active-map (kbd "C-g") 'company-abort)
  :diminish company-mode " ⓐ")

(use-package company-statistics
  :ensure t
  :after company
  :diminish company-statistics-mode)

(use-package company-quickhelp
  :ensure t
  :after company
  :init (company-quickhelp-mode))


;; python auto complete
(use-package company-anaconda
  :ensure t
  :config
  (require-install-nessary 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode)
  (setq
   python-shell-interpreter "python"
   python-shell-interpreter-args ""))

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
  (add-to-list 'popwin:special-display-config `("*magit-process*" :noselect t))
  (add-to-list 'popwin:special-display-config `"*Flycheck errors*")
  (add-to-list 'popwin:special-display-config `"*Occur*")
  (add-to-list 'popwin:special-display-config `("*Compile-Log*" :noselect t))
  (add-to-list 'popwin:special-display-config `("*Paradox Report*" :noselect t))
  (add-to-list 'popwin:special-display-config `("\\*godoc" :regexp t))
  (popwin-mode 1))


(use-package git-gutter-fringe
  :ensure t
  :config
  (when (window-system)
    (global-git-gutter-mode +1))
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default indicate-empty-lines +1)
  :diminish git-gutter-mode)

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
  :diminish ace-window-mode)

;; snippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-expand)
  :diminish (yas-minor-mode . " Ⓨ"))

;; dminish undo-tree and eldoc-mode
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

;; ycmd for emacs
(use-package ycmd
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (set-variable 'ycmd-server-command '("python" "/home/ry/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd"))
  (set-variable 'ycmd-global-config "/home/ry/.emacs.d/ycm_extra_conf.py")
  (set-variable 'ycmd-extra-conf-whitelist '("/home/ry/tunnel-agent/agentplug")))

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
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun ry/go-test()
    "test"
    (interactive)
    (let ((file (buffer-file-name)))
      (visit-term-buffer)
      (insert (format "go run %s" file))
      (comint-send-input)
      (other-window -1)))

  (define-key go-mode-map (kbd "C-c C-c") 'ry/go-test))

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; go auto complete
(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  ;; the same key as show python function doc in anaconda mode
  (define-key go-mode-map (kbd "M-?") 'godoc-at-point)
  (define-key go-mode-map (kbd "M-=") (lambda ()
                                        (interactive)
                                        (insert ":=")))
  (define-key go-mode-map (kbd "M-<") (lambda ()
                                        (interactive)
                                        (insert "<-"))))



(define-key shell-mode-map (kbd "C-n") 'comint-next-input)
(define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
(add-hook 'shell-mode-hook (lambda ()
                             (company-mode -1)
                             (yas-minor-mode -1)))


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
  (setq beacon-color "#81f7f3")
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
                                     ediff-mode
                                     )
        ;; Exclude a couple of special buffers from golden ratio, namely Helm,
        ;; WhichKey, NeoTree, etc.
        golden-ratio-exclude-buffer-regexp
        `(,(rx bos "*" (any "h" "H") "elm*" eos)
          ,(rx bos "*which-key*" eos)
          ,(rx bos "*NeoTree*" eos)))
  :diminish (golden-ratio-mode . "ⓖ"))


(use-package focus-autosave-mode        ; Save buffers when focus is lost
  :ensure t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package hl-todo                    ; Highlight TODOs in buffers
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
  :diminish which-key-mode)

(use-package dired+
  :config
  (set-face-foreground 'diredp-file-name nil))


;; start a server
(use-package server
  :config
  (or (server-running-p) (server-start)))

;; slime for lisp
(use-package slime
  :commands slime
  :init
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))
  (setq inferior-lisp-program "/usr/bin/clisp"))

;; ggtags for reading kernel code
(use-package ggtags
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

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package helm-gtags
  :ensure t)

;; when everything is set, we make our evil leader bindings
(use-package evil-leader
  :ensure t
  :config
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "l" 'linum-mode
    "w" 'save-buffer
    "q" 'kill-this-buffer
    "c SPC" 'comment-or-uncomment-line-or-region
    "b" 'bookmark-bmenu-list
    "f" 'avy-goto-word-or-subword-1
    "e" 'helm-semantic-or-imenu
    "p" 'projectile-find-file
    "g" 'magit-status
    "s" 'ag-project
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
