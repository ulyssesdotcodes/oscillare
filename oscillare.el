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

(defun oscillare-start-haskell ()
  "Start oscillare"
  (interactive)
  (call-interactively 'inferior-haskell-start-process)
  (oscillare-send-string ":module Main")
  (oscillare-send-string "(p, t, thread) <- run")
  )

(defun oscillare-quit-haskell ()
  "Quit haskell."
  (interactive)
  (kill-buffer inferior-haskell-buffer)
  (delete-other-windows))

(defun oscillare-send-string (s)
  (if (comint-check-proc inferior-haskell-buffer)
      (let ((cs (chunk-string 64 (concat s "\n"))))
        (mapcar
         (lambda (c) (comint-send-string inferior-haskell-buffer c))
         cs))
    (error "no oscillare buffer running")))

(defun oscillare-run-line ()
  "Send the current line to the haskell buffer"
  (interactive)
  (let* ((s (buffer-substring (line-beginning-position)
                              (line-end-position))))
    (oscillare-send-string s)))

(defun chunk-string (n s)
  "Split a string into chunks of n characters"
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (chunk-string n (substring s n))))))

(defun oscillare-see-output ()
  "Show output buffer"
  (interactive)
  (when (comint-check-proc inferior-haskell-buffer)
    (with-current-buffer inferior-haskell-buffer
      (let ((window (display-buffer (current-buffer))))
        (goto-char (point-max))
        (save-selected-window
          (set-window-point window (point-max)))))))

(defun oscillare-mode-keybindings (map)
  "Oscillare keybindings"
  (define-key map [?\C-c ?\C-s] 'oscillare-see-output)
  (define-key map [?\C-c ?\C-c] 'oscillare-run-line))

(defvar oscillare-mode-map nil
  "Keymap for oscillare")

(if oscillare-mode-map
    ()
  (let ((map (make-sparse-keymap "Oscillare")))
    (oscillare-mode-keybindings map)
    (setq oscillare-mode-map map)))

(define-derived-mode
  oscillare-mode
  haskell-mode
  "Oscillare"
  "Major mode for interacting with oscillare.")

(add-to-list 'auto-mode-alist '("\\.osc$" . oscillare-mode))

(provide 'oscillare)
