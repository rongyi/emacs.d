
;; using mordern emacs lib
(use-package f)
(use-package s)
(use-package dash)

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(global-set-key (kbd "s-l") (λ (insert "\u03bb")))
(global-set-key (kbd "s-f") (λ (insert "ƒ")))

(defun ry/exec (command)
  "execute a shell command and return its output as a string"
  (s-trim (shell-command-to-string command)))

(defun ry/exec-with-rc (command &rest args)
  "like ry/exec, but with return code to indicate something"
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun ry/require-or-install (pkg)
  "require a lib, if fail install it!
Use use-package instead, this function is "
  (condition-case nil
      (require pkg)
    (error
     (package-refresh-contents)
     (package-install pkg))))

;; a eval-after-load sugar
(defmacro ry/after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


;; stole this from xemacs21:
(defun ry/switch-to-other-buffer (arg)
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth (1+ arg) (buffer-list)))))

;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun ry/eshell-here ()
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


;; fast open shell init file
(defun ry/find-shell-init-file  ()
  "Edit the shell init file in another window"
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unkown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))

(defun ry/find-user-init-file ()
  "Edit the `user-init-file', in another window"
  (interactive)
  (find-file-other-window user-init-file))

(defun ry/find-user-worklog ()
  "Edit the `worklog', in another window"
  (interactive)
  (find-file-other-window "~/Documents/xxxxxxxxxx"))

(defun ry/visit-term-buffer ()
  "Create or visit a terminal buffer,
Split window first and then open zsh"
  (interactive)
  (let ((shell-name (getenv "SHELL"))
        (height (/ (window-total-height) 3)))
    (if (not (get-buffer-window shell-name))
        (progn
          (shell shell-name)
          (delete-window (selected-window))
          (split-window-vertically (- height))
          (other-window 1)
          (switch-to-buffer shell-name))
      (if (equal (buffer-name (window-buffer)) shell-name)
          (message "You already in a shell buffer!")
        (switch-to-buffer-other-window shell-name)))))

(defun ry/visit-term-buffer-with-current-dir ()
  "Simple wrapper for visit-term-buffer,
after visit also cd to the current buffer's dir"
  (interactive)
  (let ((file-name (buffer-file-name)))
    (ry/visit-term-buffer)
    (when file-name
      (insert (file-name-directory file-name))
      (comint-send-input))))


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
         (dotfile-setting (cons 83
                                100)))
    (set-frame-parameter
     frame 'alpha
     (if (not (equal alpha dotfile-setting))
         dotfile-setting
       '(100 . 100)))))

(defun ry/emacs-reload ()
  (interactive)
  (load-file user-init-file)
  (powerline-reset)
  (message ".emacs reloaded successfully"))


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

(defun ry/file-name-at-point ()
  (save-excursion
    (let* ((file-name-regexp "[./a-zA-Z0-9\-_~]")
           (start (progn
                    (while (looking-back file-name-regexp)
                      (forward-char -1))
                    (point)))
           (end (progn
                  (while (looking-at file-name-regexp)
                    (forward-char 1))
                  (point))))
      (buffer-substring start end))))

(defun ry/find-or-create-file-at-point ()
  "when under point is a path/file name, opens it."
  (interactive)
  (find-file (ry/file-name-at-point)))


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
  (let* ((file-name (or (buffer-file-name) list-buffers-directory))
         (ln (line-number-at-pos)))
    (if file-name
        (message (kill-new (format "%s:%d" file-name ln)))
      (error "Buffer not visiting a file"))))

(defun ry/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun ry/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

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
              (remove-if-not 'ry/opened-file-buffer-or-magit-p (buffer-list))))
  (message "Buffer deleted!"))

(defun ry/save-all ()
  "Saves all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))

(defun ry/close-other()
  (interactive)
  (save-excursion (other-window 1)
                  (quit-window)))

(defun ry/beautify-json (beg end)
  (interactive "r")
  (shell-command-on-region beg end "python -mjson.tool" (current-buffer) 'replace))

(defun ry/format-python ()
  (interactive)
  (when (executable-find "yapf")
    (save-excursion
      (shell-command-on-region (point-min) (point-max) "yapf" (current-buffer) 'replace))))


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

(defun ry/angry-split-switch ()
  "Switch buffer in another buffer"
  (interactive)
  (ry/split-window-horizontally-and-switch)
  (ido-switch-buffer))


;; from magnar
(defun ry/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun ry/clean-buffer ()
  "warpper for indent/untabify/delete trailing-whitespace"
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

;; from magnars modified by ffevotte for dedicated windows support
(defun ry/rotate-windows (count)
  "Rotate your windows.
Dedicated windows are left untouched. Giving a negative prefix
argument takes the kindows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))


;; from @bmag
(defun ry/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;; from lunaryorn's emacs.d
(defun ry/find-side-windows (&optional side)
  "Get all side window if any.

If SIDE is non-nil only get windows on that side."
  (let (windows)
    (walk-window-tree
     (lambda (window)
       (let ((window-side (window-parameter window 'window-side)))
         (when (and window-side (or (not side) (eq window-side side)))
           (push window windows)))))
    windows))

(defun ry/quit-all-side-windows ()
  "Quit all side windows of the current frame."
  (interactive)
  (dolist (window (ry/find-side-windows))
    (when (window-live-p window)
      (quit-window nil window)
      ;; When the window is still live, delete it
      (when (window-live-p window)
        (delete-window window)))))

(defun ry/switch-to-buffer-per-window (&optional window)
  "Switch buffer group by window"
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun ry/alternate-window ()
  "Switch back and forth between current and last window in the
current frame"
  (interactive)
  (let ((prev-window (get-mru-window nil t t)))
    (unless prev-window (user-error "Last window not found"))
    (select-window prev-window)))

;; from spacemacs, just like ours function above
(defun ry/alternate-buffer ()
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (if (evil-alternate-buffer)
      (switch-to-buffer (car (evil-alternate-buffer)))
    (switch-to-buffer (other-buffer (current-buffer) t))))

(defun ry/buffer-as-string ()
  (buffer-string (region-beginning)
                 (region-end)))

;; from http://www.emacswiki.org/emacs/WordCount
(defun ry/count-words-analysis (start end)
  "Count how many times each word is used in the region.
 Punctuation is ignored."
  (interactive "r")
  (let (words alist_words_compare (formated ""))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist_words_compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a_key (car a))
            (a_val (cdr a))
            (b_key (car b))
            (b_val (cdr b)))
        (if (eq a_val b_val)
            (string-lessp a_key b_key)
          (> a_val b_val))))
    (setq words (cl-sort words 'alist_words_compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formated (concat formated (format "[%s: %d], " name count)))))
    (when (interactive-p)
      (if (> (length formated) 2)
          (message (substring formated 0 -2))
        (message "No words.")))
    words))

(defun ry/insert-todays-date (arg)
  (interactive "P")
  (insert (format-time-string "%Y-%m-%d %A")))


(defun ry/spawn-terminal-here ()
  "Open a terminal in the current buffer's directory"
  (interactive)
  (start-process "gnome-terminal" nil "gnome-terminal"))


(defun ry/ipinfo (ip)
  "Returns the detail of an IP address from a certain IP, using ipinfo.io"
  (interactive "sEnter IP to query (blank for own IP): ")
  (request
   (concat "https://ipinfo.io/" ip)
   :headers '(("User-Agent" . "Emacs ipinfo.io Client")
              ("Accept" . "application/json")
              ("Content-Type" . "application/json;charset=utf-8"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message
                (mapconcat
                 (lambda (e)
                   (format "%10s: %s" (capitalize (symbol-name (car e))) (cdr e)))
                 data "\n"))))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Can't receive ipinfo. Error %S " error-thrown)))))

(provide 'rongyi-defun)
