;; This mode is implemented as a derivation of `haskell' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.  The directory search
;; facilities are courtesy `find-lisp'.

(require 'comint)
(require 'inf-haskell)

(defvar oscillare-buffer
  "*oscillare*"
  )


(defun oscillare-send-string (s)
  "Evaluate the code in region from BEGIN to END in the REPL.
If the region is unset, the current line will be used.
PROMPT-OPTIONS are passed to `intero-repl-buffer' if supplied."
  (let ((text (concat s "\n")))
    (intero-with-repl-buffer nil
      (comint-send-string
       (get-buffer-process (current-buffer))
       text))
    (intero-repl-switch-back)))

(defun oscillare-run-multiple-lines ()
  "Send the current region to the interpreter as a single line."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (let* ((s (buffer-substring-no-properties (region-beginning)
                                              (region-end)))
           (print-escape-newlines t))
      (oscillare-send-string ":{")
      (oscillare-send-string (replace-regexp-in-string "{Command}" (prin1-to-string s) s t t))
      (oscillare-send-string ":}")
      (mark-paragraph)
      )))

(defun oscillare-start-haskell ()
  "Start oscillare"
  (interactive)
  (intero-repl)
  (oscillare-send-string ":load src/Visuals")
  (oscillare-send-string "r <- topRunner")
  )

(defun oscillare-mode-keybindings (map)
  "Oscillare keybindings"
  (define-key map (kbd "<C-return>") 'oscillare-run-multiple-lines))

(defvar oscillare-mode-map (make-sparse-keymap)
  "Keymap for oscillare")

(define-minor-mode
  oscillare-mode
  "Minor mode for Oscillare"
  :lighter " oscillare"
  :keymap oscillare-mode-map)

(define-key oscillare-mode-map (kbd "<C-return>") 'oscillare-run-multiple-lines)


(provide 'oscillare)
