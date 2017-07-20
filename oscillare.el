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
                                              (region-end))))
      (oscillare-send-string ":{")
      (oscillare-send-string s)
      (oscillare-send-string ":}")
      (mark-paragraph)
      )))

(defun oscillare-start-haskell ()
  "Start oscillare"
  (interactive)
  (intero-repl)
  (oscillare-send-string ":load src/Visuals")
  (oscillare-send-string "r <- topRunner")
  (split-window-below-and-focus)
  )

(defun oscillare-mode-keybindings (map)
  "Oscillare keybindings"
  (define-key map (kbd "<C-return>") 'oscillare-run-multiple-lines))

(defvar oscillare-mode-map nil
  "Keymap for oscillare")

(if oscillare-mode-map
         ()
       (let ((map (make-sparse-keymap "Oscillare")))
             (oscillare-mode-keybindings map)
             (setq oscillare-mode-map map)))

(define-minor-mode
  oscillare-mode
  "Oscillare"
  "Minor mode for interacting with oscillare.")

(add-to-list 'auto-mode-alist '("\\.osc$" . oscillare-mode))

(provide 'oscillare)
