;; basic settings

;; no splash screen
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; hack and be merry, ry!")
(setq inhibit-startup-echo-area-message "rongyi")
(setq-default
 user-full-name "ry"
 user-mail-address "hiarongyi@gmail.com")
;; hightlight current line
(global-hl-line-mode)
;; custom to a seprate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; no backup file
(setq make-backup-files nil)
;; remember the cursor position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".cursor.save" user-emacs-directory))
;; smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      scroll-preserve-screen-position 1)
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
;; (defalias 'yes-or-no-p 'y-or-no-p)
;; auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq ad-redefinition-action 'accept)
;; no tab using 2spaces for tab, go mode will change this for itself
(setq-default
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2)

;; auto save
(add-hook 'focus-out-hook 'ry/save-all)
;; save every 20 characters
(setq auto-save-interval 20)

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
(set-default-coding-systems 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

(set-buffer-file-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq session-save-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Flash the frame to represent a bell.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))
;; refresh buffer fast
(setq auto-revert-interval 1)
;; no blinking cursor
(blink-cursor-mode -1)
;; make cursor the width of the character it is under
(setq x-stretch-cursor t)
;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)
;; copy mouse selected text
(setq mouse-drag-copy-region t)

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

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; highlight current line
(add-hook 'prog-mode-hook 'hl-line-mode)
;; highlight current line number
(use-package hlinum
  :ensure t
  :config
  (hlinum-activate))
;; highlight matching braces
(show-paren-mode 1)
;; highlight the entire expression
(setq show-paren-style 'expression)
;; highlight style
(custom-set-faces
 '(show-paren-match ((t (:background "azure2" :underline nil)))))

;; make copy and paste work properly under X Windows
(when (eq system-type "gnu/linux")
  (setq x-select-enable-clipboard t))

(setq truncate-partial-width-windows nil)
(setq-default truncate-lines t)
(setq-default global-visual-line-mode t)

(mouse-avoidance-mode 'exile)

;; This isn't a typewriter (even if it is a terminal); one space after sentences,
;; please.
(setq sentence-end-double-space nil)

;; font
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
;; some other font: hasklig/Source Code Pro for Powerline/Monoid/PragmataPro/Fira Code
(set-frame-font "Fira Code 10")
(add-to-list 'default-frame-alist '(font . "Fira Code 10"))
;; add some ligture: https://gist.github.com/mordocai/50783defab3c3d1650e068b4d1c91495
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 60))

(defun ry/configure-fonts (frame)
  "font configuration"
  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
  (set-fontset-font "fontset-default" 'han '("Microsoft JhengHei" . "unicode-bmp")))

(add-hook 'after-make-frame-functions
          'ry/configure-fonts)
(-when-let (frame (selected-frame))
  (ry/configure-fonts frame))

;; allow pasting selection outside of emacs
(setq select-enable-clipboard t)

;; https://gist.github.com/mordocai/50783defab3c3d1650e068b4d1c91495
(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
            ;;("\\(x\\)"                     #Xe16b)
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)

;; hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta))

(setq whitespace-style '(tailing))
(global-whitespace-mode 1)

;; subword-mode in prog-mode-hook
(add-hook 'prog-mode-hook 'subword-mode)
;; format linum, add some spaces
(setq linum-format (lambda (line)
                     (propertize
                      (format (concat " %"
                                      (number-to-string
                                       (length (number-to-string
                                                (line-number-at-pos (point-max)))))
                                      "d ")
                              line)
                      'face 'linum)))

;; Make sure script files are excutable after save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; display time
(setq display-time-24hr-format t)
(display-time-mode t)


;; ido mode
(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere t)
  (global-set-key (kbd "C-x C-f") 'ido-find-file)
  (global-set-key (kbd "C-x C-o") 'ido-find-file-other-window)

  (defun ry/ido-go-straight-home()
    (interactive)
    (if (looking-back "/")
        (insert "~/")
      (call-interactively 'self-insert-command)))

  (defun ry/setup-ido ()
    (define-key ido-file-completion-map (kbd "~") 'ry/ido-go-straight-home)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match))
  ;; go straight home by pressing ~
  (add-hook 'ido-setup-hook #'ry/setup-ido)

  (defun ido-sort-mtime ()
    "Reorder the IDO file list to sort from most recently modified."
    (setq ido-temp-list
          (sort ido-temp-list
                (lambda (a b)
                  (time-less-p
                   (sixth (file-attributes (concat ido-current-directory b)))
                   (sixth (file-attributes (concat ido-current-directory a)))))))
    (ido-to-end  ;; move . files to end (again)
     (delq nil (mapcar
                (lambda (x) (and (char-equal (string-to-char x) ?.) x))
                ido-temp-list))))
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil)

  (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
  (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package flx-ido
  :ensure t)

(setq gc-cons-threshold 20000000)
(flx-ido-mode 1)

;; eldoc-mode
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
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (setq-default smex-key-advice-ignore-menu-bar t)
  ;; change cache save place
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

;; make header file as c++mode, rarely use c now
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; highlight some keywords in comments
(add-hook 'prog-mode-hook (lambda ()
                            (font-lock-add-keywords nil
                                                    '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

(modify-syntax-entry ?_ "w")




;; add more for tab
(setq tab-always-indent 'complete)
(add-hook 'before-save-hook 'whitespace-cleanup)


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
(defvar ry/electic-pair-modes '(prog-mode org-mode text-mode))

(defun ry/inhibit-electric-pair-mode (char)
  (not (member major-mode ry/electic-pair-modes)))

(setq electric-pair-inhibit-predicate #'ry/inhibit-electric-pair-mode)


(setq-default indicate-empty-lines +1)

;; open json file in json mode
(add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . js-mode))

(setq js-indent-level 2)
;; dont intent in c++ namespace
(c-set-offset 'innamespace 0)

(ry/after-load 'org
  (require 'ox-md nil t))
;; using cl
(require 'cl)

(global-prettify-symbols-mode 1)
;; http://endlessparentheses.com/new-in-emacs-25-1-have-prettify-symbols-mode-reveal-the-symbol-at-point.html
(setq prettify-symbols-unprettify-at-point 'right-edge)

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "┌─[" 'face `(:foreground "green"))
         (propertize (user-login-name) 'face `(:foreground "red"))
         (propertize "@" 'face `(:foreground "green"))
         (propertize (system-name) 'face `(:foreground "blue"))
         (propertize "]──[" 'face `(:foreground "green"))
         (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
         (propertize "]──[" 'face `(:foreground "green"))
         (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))
         (propertize "]\n" 'face `(:foreground "green"))
         (propertize "└─>" 'face `(:foreground "green"))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green"))
         )))

;; make shell open in current buffer after emacs 25
;; https://github.com/kyagi/shell-pop-el/issues/51
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

;; don't ask process buffer when quit
;; https://emacs.stackexchange.com/questions/14509/kill-process-buffer-without-confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; global key bindings

;; file related
(global-set-key (kbd "C-c f e") 'ry/recentf-ido-find-file)
;; use ibuffer for list buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-=") 'ry/diff-buffer-file-changes)
;; switch buffer per window
(global-set-key (kbd "C-M-l") 'ry/switch-to-buffer-per-window)
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key (kbd "C-+") 'surround)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
;; take whatever we want to fit the 'combo'
(global-set-key [(control return)] 'newline-for-code)
(global-set-key (kbd "M-RET") 'newline-for-code)
(global-set-key (kbd "C-<escape>") 'insert-backquote)

(bind-key "C-c f w" 'browse-url)
(bind-key "C-c f d" 'ry/delete-current-buffer-file)
(bind-key "C-c f m" 'ry/rename-file-and-buffer)
(bind-key "C-c f c" 'ry/kill-other-buffers)
(bind-key "C-c f y" 'ry/copy-all)
(bind-key "C-c f o" 'ry/find-or-create-file-at-point)
;; like info in ghci
(bind-key "C-c f i" 'ry/show-and-copy-buffer-filename)
;; focus the function we care
(bind-key "C-c f -" 'narrow-to-defun)
(bind-key "C-c f >" 'widen)

;; edit section
(bind-key "C-c e ," 'ry/newline-after-comma)
(bind-key "C-c e d" 'ry/insert-todays-date)
(bind-key "C-c e i" 'insert-include-guard)
(bind-key "C-c e l" 'lint-code)
(bind-key "C-c e t" 'ry/delete-company-useless-template)
(bind-key "C-c e s" 'ry/sudo-edit)
(bind-key "C-c e S" 'ry/save-all)
(bind-key "C-c e w" 'toggle-truncate-lines)
(bind-key "C-c e T" 'ry/start-tmp-file)

;; put window command together
(bind-key "C-c w t" 'ry/toggle-transparency)
(bind-key "C-c w f" 'toggle-frame-maximized)

(bind-key "C-c w F" 'toggle-frame-fullscreen)
(bind-key "C-c w m" 'ry/toggle-maximize-buffer)
(bind-key "C-c w =" 'balance-windows)
(bind-key "C-c w k" 'delete-window)
;; (bind-key "C-c w \\" 'split-window-right)
;; (bind-key "C-c w -" 'split-window-below)
(bind-key "C-c w d" 'ry/toggle-current-window-dedication)
(bind-key "C-c w r" 'ry/rotate-windows)
(bind-key "M-\\" 'ry/angry-split)
(bind-key "M-|" 'ry/angry-split-switch)
(bind-key "C-c w \\" 'ry/toggle-window-split)

(bind-key "C-(" (surround-text-with "("))
(bind-key "C-\"" (surround-text-with "\""))

(define-key process-menu-mode-map (kbd "C-c k") 'ry/delete-process-at-point)


(provide 'rongyi-basic)
