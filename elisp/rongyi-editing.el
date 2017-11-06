
;; it is a unit in c/c++, and type this two key is so slow, so we need a map
(defun insert-pointer-access (prefix)
  "like := in Go, -> in C/C++ is a unit for pointer access, haskell has <-"
  (interactive "p")
  (if (> prefix 1)
      (insert "<-")
    (insert "->")))

(defun insert-backquote ()
  "Insert backquote, type ` with Leopold keyboard is a bit tedious"
  (interactive)
  (insert "`"))


(defun comment-or-uncomment-line-or-region ()
  "comments or uncomments the current line or region"
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; mainly for include guard in C/C++
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

;; https://github.com/rejeep/emacs/blob/master/rejeep-defuns.el#L150-L158
(defun join-line-or-lines-in-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

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
      (surround-text text)
      (when (not (evil-insert-state-p))
        (evil-insert-state)))))


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
  "insert link url for current buffer, the buffer name is the last section of the url"
  (interactive)
  (insert (format "// http://www.lintcode.com/zh-cn/problem/%s\n" (file-name-sans-extension (buffer-name)))))

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

;; from emacs live
(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

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

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun ry/newline-after-comma ()
  "insert newline after comma, offten used in a very long line code
especially in function argument or Python code when the lint system tell
me the line is too long"
  (interactive)
  (search-forward-regexp ", ")
  (forward-char -1)
  (delete-char 1)
  (newline-and-indent))

(defun ry/delete-company-useless-template ()
  "delete company function template"
  (interactive)
  (let ((start (make-marker))
        (end (make-marker)))
    (set-marker start (line-beginning-position))
    (set-marker end (line-end-position))
    (replace-string ", " "" nil start end)))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))


;; Duplicate start of line or region with C-M-<end>.
;; From http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))
(defun duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))
(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning) end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

;; from magnars
(defun ry/duplicate-current-line(&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (ry/duplicate-region num (point-at-bol) (1+ (point-at-eol)))))

(defun ry/duplicate-region (&optional num start end)
  "Duplicate the region bounded by START and END NUM times."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun ry/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (ry/duplicate-region arg beg end))
    (ry/duplicate-current-line arg)))

(defun ry/current-paren-count ()
  (interactive)
  (car (syntax-ppss)))

(defun ry/point-is-in-paren-p ()
  (> (current-paren-count) 0))

(defun ry/move-point-forward-out-of-paren ()
  (interactive)
  (while (point-is-in-paren-p) (forward-char)))

(defun ry/move-point-backward-out-of-paren ()
  (interactive)
  (while (point-is-in-paren-p) (backward-char)))

(defun ry/strip-paren ()
  "Strip paren, (argument) ==> argument"
  (interactive)
  (if (point-is-in-paren-p)
      (save-excursion
        (move-point-forward-out-of-paren)
        (backward-delete-char 1)
        (move-point-backward-out-of-paren)
        (delete-char 1))
    (error "Point isn't in paren")))

(defun ry/kill-and-indent-line ()
  "Kill the entire current line and reposition point at indentation, just like Vim's S command"
  (interactive)
  (back-to-indentation)
  (kill-line))

(defun ry/comment-kill-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos)))))

(defun ry/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

;; from spacemacs
(defun current-line ()
  "Return the line at point as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun ry/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

;; http://stackoverflow.com/a/10216338/4869
(defun ry/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (if (region-active-p)
      (clipboard-kill-ring-save (region-beginning)
                                (region-end))
    (clipboard-kill-ring-save (point-min) (point-max))))

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

(defun start--file (path)
  "Create a file at PATH, creating any containing directories as necessary.
Visit the file after creation."
  (make-directory (file-name-directory path) t)
  (find-file path))

(defun ry/start-tmp-file (file-name)
  "Create a file in /tmp for the given file name."
  (interactive "sName of temporary file: ")
  (start--file (expand-file-name (format "/tmp/%s" file-name))))

(defun ry/start-scratch-html-file (file-name)
  "Create a test HTML file in ~/scratch to play around with."
  (interactive "sName of scratch HTML file: ")
  (ry/start-tmp-file file-name)
  (erase-buffer)
  (insert "<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
        <title>
        </title>
        <style type=\"text/css\">
        </style>
    </head>
    <body>

    </body>
</html>")
  (forward-line -2)
  (move-end-of-line nil))

(provide 'rongyi-editing)
