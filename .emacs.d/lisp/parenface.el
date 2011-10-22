;;; parenface.el --- Provide a face for parens in lisp modes.
;; By Dave Pearson <davep@davep.org>
;; $Revision: 1.1 $

;; Add a paren-face to emacs and add support for it to the various lisp modes.
;;
;; Based on some code that Boris Schaefer <boris@uncommon-sense.net> posted
;; to comp.lang.scheme in message <87hf8g9nw5.fsf@qiwi.uncommon-sense.net>.

(defvar paren-face 'paren-face)

(defvar paren-face-light "#93a1a1"      ;base1
  "Face for parentheses against a light background (see background-mode).")
(defvar paren-face-dark  "#586e75"      ;base01
  "Face for parentheses against a dark background (see background-mode).")

(defface paren-face
    '((((class color))
       (:foreground "Gray70")))         ;was DimGray
  "Face for displaying a paren."
  :group 'faces)

(defmacro paren-face-add-support (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
    (let* ((regexp "(\\|)")
           (match (assoc regexp ,keywords)))
      (unless (eq (cdr match) paren-face)
        (setq ,keywords (append (list (cons regexp paren-face)) ,keywords))))))

;; Keep the compiler quiet.
(eval-when-compile
  (defvar scheme-font-lock-keywords-2 nil)
  (defvar lisp-font-lock-keywords-2 nil))

(add-hook 'scheme-mode-hook           (paren-face-add-support scheme-font-lock-keywords-2))
(add-hook 'lisp-mode-hook             (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'emacs-lisp-mode-hook       (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'lisp-interaction-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))

(defun adjust-paren-face-fg ()
  "Set the paren-face :foreground according to the frame-bacground-mode (light,
dark or nil)."
  (interactive)
  (let ((paren-face-fg (case frame-background-mode
                         (light
                          paren-face-light)
                         (dark
                          paren-face-dark)
                         (t             ;if not set yet
                          paren-face-light))))
    (set-face-attribute 'paren-face nil :foreground paren-face-fg)))

(provide 'parenface)

;; parenface.el ends here