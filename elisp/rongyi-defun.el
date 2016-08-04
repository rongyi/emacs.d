

(defun require-install-nessary (pkg)
  (condition-case nil
      (require pkg)
    (error
     (package-refresh-contents)
     (package-install pkg))))

;; a eval-after-load sugar
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


;; it is a unit in c/c++, and type this two key is so slow, so we need a map
(defun insert-pointer-access ()
  (interactive)
  (insert "->"))

(defun my-helm-in-ido (buffer)
  "Display a helm buffer in ido. Send the purists screaming."
  (interactive)
  (ido-buffer-internal 'display 'display-buffer nil nil nil 'ignore))

(defun comment-or-uncomment-line-or-region ()
  "comments or uncomments the current line or region"
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))


(defun random-suffix ()
  (let ((ret "")
        (mycharset "1234567890ABCDEFGHIJKLMNOPQRSTYVWXYZ"))
    (dotimes (i 8)
      (let ((idx (random (length mycharset))))
        (setq ret (concat ret (substring mycharset idx (1+ idx))))))
    ret))

(defun insert-include-guard()
  (interactive)
  (let ((prefix (concat
                 (replace-regexp-in-string "[.-]" "_" (upcase (file-name-sans-extension (buffer-name))))
                 "_"
                 (random-suffix)
                 "_H")))
    (save-excursion
      (beginning-of-buffer)
      (insert (concat "#ifndef " prefix "\n"))
      (insert (concat "#define " prefix "\n"))
      (end-of-buffer)
      (insert "\n#endif /* include guard end */\n"))))


;; http://slashusr.wordpress.com/2010/01/19/quickly-diff-the-changes-made-in-the-current-buffer-with-its-file/
(defun ry/diff-buffer-file-changes ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))
;;; this over-rides 'text-scale-adjust, but that's also available on C-x C-+:

;; stole this from xemacs21:
(defun switch-to-other-buffer (arg)
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth (1+ arg) (buffer-list)))))



;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)
    (company-mode -1)))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))


(defun newline-for-code ()
  "Inserts a newline character, but from the end of the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))



(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line

If arg is not nill or 1, move forward ARG - 1 lines first."
  (interactive "^p")
  (setq arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


;; from howard abram
(defun surround (start end txt)
  "Wraps the specified region (or the current 'symbol / word'
     with some textual markers that this function requests from the
     user. Opening-type text, like parens and angle-brackets will
     insert the matching closing symbol.

     This function also supports some org-mode wrappers:

       - `#s` wraps the region in a source code block
       - `#e` wraps it in an example block
       - `#q` wraps it in an quote block"
  (interactive "r\nsEnter text to surround: " start end txt)

  ;; If the region is not active, we use the 'thing-at-point' function
  ;; to get a "symbol" (often a variable or a single word in text),
  ;; and use that as our region.

  (if (not (region-active-p))
      (let ((new-region (bounds-of-thing-at-point 'symbol)))
        (setq start (car new-region))
        (setq end (cdr new-region))))

  ;; We create a table of "odd balls" where the front and the end are
  ;; not the same string.
  (let* ((s-table '(("#e" . ("#+BEGIN_EXAMPLE\n" "\n#+END_EXAMPLE") )
                    ("#s" . ("#+BEGIN_SRC \n"    "\n#+END_SRC") )
                    ("#q" . ("#+BEGIN_QUOTE\n"   "\n#+END_QUOTE"))
                    ("<"  . ("<" ">"))
                    ("("  . ("(" ")"))
                    ("{"  . ("{" "}"))
                    ("["  . ("[" "]"))))    ; Why yes, we'll add more
         (s-pair (assoc-default txt s-table)))

    ;; If txt doesn't match a table entry, then the pair will just be
    ;; the text for both the front and the back...
    (unless s-pair
      (setq s-pair (list txt txt)))

    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert (car s-pair))
      (goto-char (point-max))
      (insert (cadr s-pair))
      (widen))))

(defun surround-text (txt)
  (if (region-active-p)
      (surround (region-beginning) (region-end) txt)
    (surround nil nil txt)))

(defun surround-text-with (surr-str)
  "Returns an interactive function that when called, will surround the region (or word) with the SURR-STR string."
  (lexical-let ((text surr-str))
    (lambda ()
      (interactive)
      (surround-text text))))

;; steal from prelude
(defun ry/open-line-above ()
  "insert an empty line above current line"
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))


(defun ry/open-line-below ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun lint-code ()
  (interactive)
  (insert (format "// http://www.lintcode.com/zh-cn/problem/%s\n" (file-name-sans-extension (buffer-name)))))


;; fast open shell init file
(defun find-shell-init-file  ()
  "Edit the shell init file in another window"
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unkown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window"
  (interactive)
  (find-file-other-window user-init-file))


(defun visit-term-buffer ()
  "Create or visit a terminal buffer"
  (interactive)
  (let ((shell-name (getenv "SHELL"))
        (height (/ (window-total-height) 3)))
    (if (not (get-buffer-window shell-name))
        (progn
          (split-window-vertically (- height))
          (other-window 1)
          (if (not (get-buffer shell-name))
              (shell shell-name)
            (switch-to-buffer shell-name)))
      (if (equal (buffer-name (window-buffer)) shell-name)
          (message "You already in a shell buffer!")
        (switch-to-buffer-other-window shell-name)))))

(defun visit-term-buffer-with-current-dir ()
  "Simple wrapper for visit-term-buffer,
after visit also cd to the current buffer's dir"
  (interactive)
  (let ((file-name (buffer-file-name)))
    (visit-term-buffer)
    (when file-name
      (insert (file-name-directory file-name))
      (comint-send-input))))

;; indent utility
(defun indent-defun()
  "Ident the current defun"
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun indent-buffer ()
  "Indent the currently visited buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer"
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer")))))

(defun ry/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))


(defun ry/find-file-as-root ()
  "Edit a file as root"
  (interactive)
  (let ((file (ido-read-file-name "Edit as root:")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun ry/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(defun ry/toggle-transparency ()
  "Toggle between transparent or opaque display."
  (interactive)
  (let* ((frame (selected-frame))
         (alpha (frame-parameter frame 'alpha))
         (dotfile-setting (cons 80
                                100)))
    (set-frame-parameter
     frame 'alpha
     (if (not (equal alpha dotfile-setting))
         dotfile-setting
       '(100 . 100)))))

(defun ry/emacs-reload ()
  (interactive)
  (load-file user-init-file)
  (message ".emacs reloaded successfully")
  (powerline-reset))


(defun ry/eval-current-form ()
  "Looks for the current def* or set* command then evaluates, unlike `eval-defun', does not go to topmost function"
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

;; from bodil's emacs.d
(defun ry/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; from https://gist.github.com/3402786
(defun ry/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

;; from magnars
(defun ry/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; from magnars
(defun ry/sudo-edit (&optional arg)
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name)) (read-file-name "File: ") buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname) last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))


;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun ry/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))


;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun ry/show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun ry/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun ry/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

;; http://stackoverflow.com/a/10216338/4869
(defun ry/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun ry/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun ry/sort-lines ()
  "Sort lines in region or current buffer"
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))


(defvar user-home-directory (concat (expand-file-name "~") "/"))

(defun ry/shorter-file-name (file-name)
  (s-chop-prefix user-home-directory file-name))

(defun ry/recentf--file-cons (file-name)
  (cons (ry/shorter-file-name file-name) file-name))

(defun ry/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let* ((recent-files (mapcar 'ry/recentf--file-cons recentf-list))
         (files (mapcar 'car recent-files))
         (file (completing-read "Choose recent file: " files)))
    (find-file (cdr (assoc file recent-files)))))


;; from howard abrams
(defun filter (condp lst)
  "Emacs Lisp doesn’t come with a ‘filter’ function to keep
    elements that satisfy a conditional and excise the elements that
    do not satisfy it. One can use ‘mapcar’ to iterate over a list
    with a conditional, and then use ‘delq’ to remove the ‘nil’
    values."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun reject (condp lst)
  "reverse of filter"
  (delq nil
        (mapcar (lambda (x) (and (not (funcall condp x )) x))
                lst)))

(defun ry/opened-file-buffer-or-magit-p (buffer)
  "return true if this buffer is a opened file or a magit buffer"
  (or (buffer-file-name buffer) (string-prefix-p "*magit" (buffer-name buffer))))

;; http://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun ry/kill-other-buffers ()
  "Kill all other buffers(with file opened)."
  (interactive)
  (save-some-buffers t)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'ry/opened-file-buffer-or-magit-p (buffer-list)))))

(defun save-all ()
  "Saves all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))

(defun beautify-json (beg end)
  (interactive "r")
  (shell-command-on-region beg end "python -mjson.tool" (current-buffer) 'replace))

(defun ry/format-python ()
  (interactive)
  (when (executable-find "yapf")
    (save-excursion
      (shell-command-on-region (point-min) (point-max) "yapf" (current-buffer) 'replace))))


;; from emacs live
(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))


;; seen at http://stackoverflow.com/a/18034042
(defun ry/delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

(defun ry/copy-all ()
  "Copy the entire buffer to the clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun ry/scratch ()
  "Create a new scratch buffer that does not need to be
saved. This is useful for editing snippets of text in a temporary
buffer"
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun ry/angry-split ()
  "Open file after split"
  (interactive)
  (ry/split-window-horizontally-and-switch)
  (ido-find-file))


;; http://sriramkswamy.github.io/dotemacs/
(defun ry/replace-next-underscore-with-camel (arg)
  (interactive "p")
  (if (> arg 0)
 (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (let ((case-fold-search nil))
    (while (not (= arg 1))
      (search-forward-regexp "\\b_[a-z]")
      (forward-char -2)
      (delete-char 1)
      (capitalize-word 1)
      (setq arg (1- arg)))))

(provide 'rongyi-defun)
