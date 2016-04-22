;; init file

;; debugging
(setq message-log-max 100000)
;; create dir and add it to load path
(defconst ry/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun ry/emacs-subdirectory (d)
  (expand-file-name d ry/emacs-directory))

(let* ((subdirs '("elisp"))
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

;; Our settings
(require 'rongyi-defun)
(require 'rongyi-basic)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)



(use-package evil
  :init
  (setq evil-want-C-i-jump nil)
  :ensure t
  :config

  (define-key evil-insert-state-map (kbd "M-.") 'insert-pointer-access)
  (define-key evil-insert-state-map "\C-c" '(lambda ()
                                              (interactive)
                                              (save-excursion
                                                (evil-normal-state)
                                                (when (fboundp 'company-abort)
                                                  (company-abort))
                                                )))
  (define-key evil-visual-state-map "\C-c" 'evil-normal-state)
  (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-normal-state-map "\C-a" 'evil-beginning-of-line)
  (define-key evil-insert-state-map "\C-a" 'evil-beginning-of-line)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-insert-state-map "\C-s" 'save-buffer)
  (define-key evil-insert-state-map "\C-k" 'kill-line)
  (define-key evil-normal-state-map "\C-s" 'save-buffer)
  (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-normal-state-map "\C-f" 'evil-scroll-page-down)
  (define-key evil-insert-state-map "\C-f" 'forward-char)
  (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
  (define-key evil-normal-state-map "\C-b" 'evil-scroll-page-up)
  (define-key evil-insert-state-map "\C-b" 'backward-char)
  (define-key evil-visual-state-map "\C-b" 'evil-backward-char)
  (define-key evil-normal-state-map "\C-d" 'evil-delete-char)
  (define-key evil-insert-state-map "\C-d" 'evil-delete-char)
  (define-key evil-visual-state-map "\C-d" 'evil-delete-char)
  (define-key evil-normal-state-map "\C-n" 'evil-next-line)
  (define-key evil-insert-state-map "\C-n" 'evil-next-line)
  (define-key evil-visual-state-map "\C-n" 'evil-next-line)
  (define-key evil-normal-state-map "\C-p" 'evil-previous-line)
  (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
  (define-key evil-visual-state-map "\C-p" 'evil-previous-line)
  (define-key evil-normal-state-map "\C-w" 'evil-delete)
  (define-key evil-insert-state-map "\C-w" 'evil-delete)
  (define-key evil-visual-state-map "\C-w" 'evil-delete)

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

  :diminish evil-mode
  )

(use-package evil-anzu
  :ensure t)


;; evil leader
(use-package evil-leader
  :ensure t
  :config
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "l" 'linum-mode
    "w" 'save-buffer)
  )

;; avy
(require-install-nessary 'avy)
(evil-leader/set-key "f" 'avy-goto-word-or-subword-1)

;; expand-region
(require-install-nessary 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; helm
(require-install-nessary 'helm)
(require-install-nessary 'helm-config)
(require-install-nessary 'helm-misc)
(require-install-nessary 'helm-locate)
(require-install-nessary 'projectile)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
;;(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-buffers-fuzzy-matching t)
(setq helm-split-window-default-side (quote other))
(setq helm-split-window-in-side-p nil)
(setq helm-display-function 'helm-default-display-buffer)
(setq helm-adaptive-history-file (expand-file-name
          "helm-adapative-history"
          user-emacs-directory))

(evil-leader/set-key "e" 'helm-semantic-or-imenu)

;; magit
(require-install-nessary 'magit)
(evil-leader/set-key "g" 'magit-status)
(global-set-key (kbd "<f2>") 'magit-status)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-commit-arguments '("--verbose"))
;; to be tested
;;(require-install-nessary 'magit-find-file)


;; powerline
(require-install-nessary 'powerline)
(setq powerline-default-separator 'wave)
(powerline-center-evil-theme)

;; display time
(setq display-time-24hr-format t)
(display-time-mode t)

;; flycheck
(require-install-nessary 'flycheck)
(require-install-nessary 'flycheck-pos-tip)
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-mode t)

(with-eval-after-load 'flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
  (setq flycheck-standard-error-navigation nil))

;; flycheck errors on a tooltip (doesnt work on console)
(when (display-graphic-p (selected-frame))
  (with-eval-after-load 'flycheck
    (custom-set-variables
     '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))


;; silver searcher
(require-install-nessary 'ag)
(evil-leader/set-key "s" 'ag)


;; color variable
(require-install-nessary 'color-identifiers-mode)
(global-color-identifiers-mode)

;; rainbow delimeters
(require-install-nessary 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; smartparen

(require-install-nessary 'smartparens)
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)

;; company
(require-install-nessary 'company)
(require-install-nessary 'company-statistics)
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

;; python auto complete
(require-install-nessary 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(setq
 python-shell-interpreter "python"
 python-shell-interpreter-args "")
;; js

(require-install-nessary 'js2-mode)
(require-install-nessary 'company-tern)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)
(setq company-tern-property-marker "")
(setq company-tern-meta-as-single-line t)
(after-load 'js2-mode
  (setq js2-highlight-level 3
        js2-basic-offset 2
        js2-pretty-multiline-declarations t))
(require-install-nessary 'json-mode)



;; ido mode
(after-load 'ido
  (ido-mode t)
  (ido-everywhere t))
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(require-install-nessary 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(require-install-nessary 'ido-vertical-mode)
(ido-vertical-mode)
(require-install-nessary 'flx-ido)
(setq gc-cons-threshold 20000000)
(flx-ido-mode 1)

(add-hook 'ido-setup-hook (lambda ()
                            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
                            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))



;; eldoc-mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; org-mode setting
(setq org-startup-folded nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
(require-install-nessary 'org-bullets)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)))

;; set shell coding
(defadvice ansi-term (after ry/advise-ansi-term-coding-system activate)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

;; close buffer when quit shell
(defadvice term-sentinel (around ry/advice-term-sentinel (proc msg) activate)
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))

;; ediff option

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; export shell path
(require-install-nessary 'exec-path-from-shell)
(when (and (eq system-type 'darwin) (display-graphic-p))
  (require-install-nessary 'exec-path-from-shell)
  (setq exec-path-from-shell-variables '("PATH"  "MANPATH" "SHELL" "GOPATH"))
  (exec-path-from-shell-initialize))


;; smex
(require-install-nessary 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq-default smex-key-advice-ignore-menu-bar t)
;; change cache save place
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))

;; close unnessary buffer automaticly
(require-install-nessary 'popwin)

(after-load 'popwin
  (add-to-list 'popwin:special-display-config `"*ag search*")
  (add-to-list 'popwin:special-display-config `("*magit-process*" :noselect t))
  (add-to-list 'popwin:special-display-config `"*Flycheck errors*")
  (add-to-list 'popwin:special-display-config `"*Occur*")
  (add-to-list 'popwin:special-display-config `("*Compile-Log*" :noselect t))
  (add-to-list 'popwin:special-display-config `("*Paradox Report*" :noselect t))
  (add-to-list 'popwin:special-display-config `("\\*godoc" :regexp t)))
(popwin-mode 1)


(when (window-system)
  (require-install-nessary 'git-gutter-fringe))
(global-git-gutter-mode +1)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; ethan-wspace
(require-install-nessary 'ethan-wspace)
(setq mode-require-final-newline nil
      require-final-newline nil)
(global-ethan-wspace-mode 1)
(evil-leader/set-key
  "SPC" 'ethan-wspace-clean-all)

;; Enhance C-x o when more than two window are open
(require-install-nessary 'switch-window)
(require-install-nessary 'ace-window)
;; the key "combo" is fast than the least used C-x o
;; so we decide to make a change with swap hot key
(global-set-key (kbd "C-x C-o") 'ace-window)
(evil-leader/set-key "w" 'ace-window)
;; it seems like we dont need swap window frequently
(global-set-key (kbd "C-x o") 'ace-window)
(evil-leader/set-key "K" (lambda ()
                           (interactive)
                           (save-excursion
                             (other-window 1)
                             (quit-window)
                             (other-window 1))))

;; snippet
;; (require-install-nessary 'yasnippet)
;; (yas-global-mode 1)
;; (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-expand)


(evil-leader/set-key "c SPC" 'comment-or-uncomment-line-or-region)
;; ycmd for emacs
(require-install-nessary 'ycmd)
(add-hook 'c++-mode-hook 'ycmd-mode)
(set-variable 'ycmd-server-command '("python" "/home/ry/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd"))
(set-variable 'ycmd-global-config "/home/ry/.emacs.d/ycm_extra_conf.py")
(set-variable 'ycmd-extra-conf-whitelist '("/home/ry/tunnel-agent/agentplug"))

(require-install-nessary 'company-ycmd)
(company-ycmd-setup)
(require-install-nessary 'flycheck-ycmd)
(flycheck-ycmd-setup)

;; make header file c++mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

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



;; highlight TODO
(add-hook 'prog-mode-hook (lambda ()
                            (font-lock-add-keywords nil
                                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
(modify-syntax-entry ?_ "w")

(global-set-key (kbd "C-x C-=") 'ry/diff-buffer-file-changes)


;; bind to C-M-l, just like in xemacs:
(global-set-key (kbd "C-M-l") 'switch-to-other-buffer)


(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(global-set-key (kbd "C-+") 'surround)

(setq inferior-lisp-program "/usr/bin/clisp")

(global-set-key (kbd "C-x f") 'toggle-frame-maximized)

(define-key evil-insert-state-map (kbd "C-o") 'ry/open-line-above)

;; golang config
(require-install-nessary 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
;; go auto complete
(require-install-nessary 'company-go)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))
;; the same key as show python function doc in anaconda mode
(define-key go-mode-map (kbd "M-?") 'godoc-at-point)
(require-install-nessary 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(define-key go-mode-map (kbd "M-=") (lambda ()
                                      (interactive)
                                      (insert ":=")))
(define-key go-mode-map (kbd "M-<") (lambda ()
                                      (interactive)
                                      (insert "<-")))
(define-key shell-mode-map (kbd "C-n") 'comint-next-input)
(define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
(add-hook 'shell-mode-hook (lambda ()
                             (company-mode -1)
                             (yas-minor-mode -1)))

;; add more for tab
(setq tab-always-indent 'complete)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
