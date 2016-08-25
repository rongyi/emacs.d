
;; it is a unit in c/c++, and type this two key is so slow, so we need a map
(defun insert-pointer-access ()
  "like := in Go, -> in C/C++ is a unit for pointer access"
  (interactive)
  (insert "->"))


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

(provide 'rongyi-editing)
