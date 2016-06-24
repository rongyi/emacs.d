;; basic settings


;; basic settings
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; happy hacking, ry")
(setq inhibit-startup-echo-area-message "rongyi")
;; custom to a seprate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; no backup file
(setq make-backup-files nil)
;; remember the cursor position
(setq save-place-file (expand-file-name "cursor.save" user-emacs-directory))
(setq-default save-place t)
(require 'saveplace)
;; smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      scroll-preserve-screen-position 1
      redisplay-dont-pause t)
;; mouse scroll
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; full screen if needed
;;(toggle-frame-fullscreen)
(toggle-frame-maximized)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))
;; yes-or-no-p ==> y-or-n
;; (fset 'yes-or-no-p 'y-or-no-p)
;; auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq ad-redefinition-action 'accept)
;; no tab using 2spaces for tab
(setq-default
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2)

;; break long lines at word boundaries
(visual-line-mode 1)

;; Enable the mouse in terminal mode.
(xterm-mouse-mode 1)

;; syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
;; line mode
(line-number-mode 1)
(column-number-mode 1)
;; show the modifier combination I just typed almost immediately
(setq echo-keystrokes 0.1)

;; UTF-8 everything!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Flash the frame to represent a bell.
(setq visible-bell t)
;; nevermind that's annoying
(setq ring-bell-function 'ignore)
;; refresh buffer fast
(setq auto-revert-interval 1)
;; Show me the new saved file if the contents change on disk when editing.
(global-auto-revert-mode 1)
;; no blinking cursor
(blink-cursor-mode -1)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode 1)

;; show current function in modeline
(which-function-mode)
;; show column numbers in modline
(setq column-number-mode t)

;; use ibuffer for list buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
;; (require 'whitespace)
;; (setq whitespace-line-column 80)
;; (setq whitespace-style '(face lines-tail))

;; (add-hook 'prog-mode-hook 'whitespace-mode)
;; highlight the word under the point
;;(add-hook 'prog-mode-hook 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
;; highlight current line number
(require-install-nessary 'hlinum)
(hlinum-activate)
;; highlight matching braces
(show-paren-mode 1)
;; highlight the entire expression
(setq show-paren-style 'expression)
;; highlight style
(custom-set-faces
 '(show-paren-match ((t (:background "azure2")))))

;; make copy and paste work properly under X Windows
(when (eq system-type "gnu/linux")
  (setq x-select-enable-clipboard t))

(setq truncate-partial-width-windows nil)


(mouse-avoidance-mode 'exile)

;; This isn't a typewriter (even if it is a terminal); one space after sentences,
;; please.
(setq sentence-end-double-space nil)
;; font
(set-frame-font "Source Code Pro for Powerline 10")
(add-to-list 'default-frame-alist '(font . "Source Code Pro for Powerline 10"))
(add-hook 'after-make-frame-functions
          (lambda (new-frame)
            (set-fontset-font "fontset-default" 'han '("Microsoft JhengHei" . "unicode-bmp"))
            ))
(set-fontset-font "fontset-default" 'han '("Microsoft JhengHei" . "unicode-bmp"))

;; my poor child, your mac air is so small
(when (eq system-type 'darwin)
  (set-frame-font "Source Code Pro for Powerline 12")
  (add-to-list 'default-frame-alist '(font . "Source Code Pro for Powerline 12"))
  (add-hook 'after-make-frame-functions
            (lambda (new-frame)
              (set-fontset-font "fontset-default" 'han '("Microsoft JhengHei" . "unicode-bmp"))
              ))
  (set-fontset-font "fontset-default" 'han '("Microsoft JhengHei" . "unicode-bmp")))

;; hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; (load-theme 'leuven)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  )

(setq whitespace-style '(tailing))
(global-whitespace-mode 1)

;; subword-mode in prog-mode-hook
(add-hook 'prog-mode-hook 'subword-mode)
;; format linum
(setq linum-format (if (not window-system) "%4d " "%4d"))

;; Make sure script files are excutable after save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; display time
(setq display-time-24hr-format t)
(display-time-mode t)


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

;; shell
(when (eq system-type 'darwin)
  (setq explicit-shell-file-name "/bin/zsh"))
;; ediff option

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; smex
(require-install-nessary 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq-default smex-key-advice-ignore-menu-bar t)
;; change cache save place
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))

;; make header file c++mode, for current job
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; highlight TODO
(add-hook 'prog-mode-hook (lambda ()
                            (font-lock-add-keywords nil
                                                    '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

(modify-syntax-entry ?_ "w")

(global-set-key (kbd "C-x C-=") 'ry/diff-buffer-file-changes)


;; bind to C-M-l, just like in xemacs:
(global-set-key (kbd "C-M-l") 'switch-to-other-buffer)


(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(global-set-key (kbd "C-+") 'surround)

(global-set-key (kbd "C-x f") 'toggle-frame-maximized)


;; add more for tab
(setq tab-always-indent 'complete)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)


;; When not in a terminal, configure a few window system specific things.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(custom-set-faces
 '(header-line
   ((default
      :inherit mode-line)
    (((class color grayscale) (background light))
     :background "black" :foreground "grey20" :box nil)
    )))

(electric-pair-mode 1)

;; but disable electric-pair-mode in minibuffer
;; http://emacs.stackexchange.com/questions/5981/how-to-make-electric-pair-mode-buffer-local/5990#5990
(defvar ry-electic-pair-modes '(prog-mode org-mode text-mode))

(defun ry-inhibit-electric-pair-mode (char)
  (not (member major-mode ry-electic-pair-modes)))

(setq electric-pair-inhibit-predicate #'ry-inhibit-electric-pair-mode)

;; take whatever we want to fit the 'combo'
(global-set-key [(control return)] 'newline-for-code)
(global-set-key (kbd "M-RET") 'newline-for-code)

(setq-default indicate-empty-lines +1)

;; open json file in json mode
(add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . js-mode))

(setq js-indent-level 2)

(after-load 'org
  (require 'ox-md nil t))


(provide 'rongyi-basic)
