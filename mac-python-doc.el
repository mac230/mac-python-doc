(defun mac-python-help-doc ()
  "Print the result of calling 'help(symbol)' in python to a temporary
help buffer.  If point is at a symbol, call the documentation for that
symbol.  Otherwise, prompt for a symbol via 'read-string'.  Once the
documentation buffer has been displayed, return point to the previous
window and location in the current buffer."
  (interactive)
(let ((current-window (selected-window))
      (current-point (point))
      (python-help-buffer (get-buffer-create "*Python-Help*"))
      (python-symbol (symbol-at-point))
      (python-string))
  (when (not python-symbol)
    (setq python-symbol (read-string "help on: ")))
  (setq python-string (concat "help('" python-symbol "')"))
  (with-output-to-temp-buffer
      (prin1 (python-shell-send-string-no-output python-string) python-help-buffer))
  (pop-to-buffer python-help-buffer)
  (beginning-of-buffer)
  (help-mode)
  (select-window current-window)
  (goto-char current-point))
)
