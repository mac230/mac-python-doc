;; functions for context-aware access to documentation pages (man, help, etc...)
;; -----
(defun mac-man-page ()
  "Get a manpage while editing bash code."
  (interactive)
  (woman
   (ivy-completing-read
    "man page for: "
    (woman-topic-all-completions woman-expanded-directory-path))))


;; -----
;; for python completion
;; note that this requires 'epc'; if you don't have it, 'pip install epc'
(defun jedi:ac-direct-matches ()
  (mapcar
   (lambda (x)
     (cl-destructuring-bind (&key word doc description symbol)
         x
       (popup-make-item word
                        :symbol symbol
                        :document (unless (equal doc "") doc)
                        :summary description)))
   jedi:complete-reply))


;; -----
(defun mac-python-help-doc ()
  "Print the result of calling 'help(symbol)' in python to a temporary
help buffer.  If point is at a symbol, call the documentation for that
symbol.  Otherwise, prompt for a symbol via 'read-string'.  Once the
documentation buffer has been displayed, return point to the previous
window and location in the current buffer."
  (interactive)
(let ((python-help-buffer (get-buffer-create "*Python-Help*"))
      (python-symbol (symbol-at-point))
      (python-string))

(when (not python-symbol)
  (setq python-symbol
        (completing-read "Help on: " (jedi:ac-direct-matches))))

  (setq python-string
        (concat "help('"
                (format "%s" python-symbol)
                "')"))

  (with-output-to-temp-buffer python-help-buffer
    (prin1 (python-shell-send-string-no-output python-string) python-help-buffer))

(pop-to-buffer python-help-buffer)
(beginning-of-buffer)
(help-mode)
))




;; -----
(defun mac-master-documentation (arg)
  "My function for context-aware calling of documentation for R, python, shell, or elisp.
  Relies on various helper functions."
  (interactive "P")
  (let ((cb (current-buffer))
        (cp (point))
        (completions '(("ess-mode" ess-mode) ("sh-mode" sh-mode) ("python-mode" python-mode) ("emacs-lisp-mode" emacs-lisp-mode)))
        (mm major-mode)
        (topic (thing-at-point 'word t)))

    (when (not (bufferp (get-buffer "*R*")))
      (rstart))

    (when (or
	   (eq mm 'org-mode)
	   (not (= (prefix-numeric-value arg) 1)))
      (setq mm (cadr (assoc (completing-read "mode: " completions) completions))))

    (cond
     ;; R help - use "*R*" as current buffer to get completions
     ((eq mm 'ess-mode)
        (if (or
             (not topic)
             ;; deal with thing-at-point not handling "." containing sexps
             ;; need to double the 'with-current-buffer' or the looking-at test runs in R
             (or (looking-at "[[:alnum:]]+[._]")
                 (looking-back "[[:alnum:]]+[._][[:alnum:]]+")))
                 (with-current-buffer "*R*"
                 (call-interactively 'ess-display-help-on-object))
                 (with-current-buffer "*R*"
                 (ess-display-help-on-object topic))))

     ;; python help
     ((eq mm 'python-mode)
      (progn
        (mac-python-help-doc)))

     ;; shell help
     ((or
       (eq mm 'sh-mode)
       (eq mm 'shell-mode)
       (eq mm 'eshell-mode))
       (mac-man-page))

     ;; elisp help
     (t
      (if (functionp (symbol-at-point))
          (describe-function (symbol-at-point))
      (call-interactively 'describe-function)))
     )

    (unless
        (eq (window-buffer) cb)
      (progn
        (switch-to-buffer-other-window cb)
        (goto-char cp)))
    ))
