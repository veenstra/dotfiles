;; -*-Emacs-Lisp-*-
;;
;; ruby-electric.el --- electric commands editing for ruby files
;;
;; Copyright (C) 2005 by Dee Zsombor <dee dot zsombor at gmail dot com>.
;; Released under same license terms as Ruby.
;;
;; Due credit: this work was inspired by a code snippet posted by
;; Frederick Ros at http://rubygarden.org/ruby?EmacsExtensions.
;;
;; Following improvements where added:
;;
;;       - handling of strings of type 'here document'
;;       - more keywords, with special handling for 'do'
;;       - packaged into a minor mode

;; Modified 2008 by Robert Elm <rodimius@gmail.com>
;; Added support for my clumsy typing.
;; So typing "(" then something else then ")" overwrites the
;; closing bracket if I forgot about ruby-electric.
;; This works for all the single character electric keys, but
;; not for keywords
;;
;; Usage:
;;
;;    0) copy ruby-electric.el into directory where emacs can find it.
;;
;;    1) modify your startup file (.emacs or whatever) by adding
;;       following line:
;;
;;            (require 'ruby-electric)
;;
;;    2) toggle Ruby Electric Mode on/off with ruby-electric-mode.
;;
;; Changelog:
;;
;;  2005/Jan/14: inserts matching pair delimiters like {, [, (, ', ",
;;  ' and | .
;;
;;  2005/Jan/14: added basic Custom support for configuring keywords
;;  with electric closing.
;;
;;  2005/Jan/18: more Custom support for configuring characters for
;;  which matching expansion should occur.
;;
;;  2005/Jan/18: no longer uses 'looking-back' or regexp character
;;  classes like [:space:] since they are not implemented on XEmacs.
;;
;;  2005/Feb/01: explicitly provide default argument of 1 to
;;  'backward-word' as it requires it on Emacs 21.3



(require 'ruby-mode)

(defconst ruby-electric-expandable-do-or-begin-re
  "\\(do\\|begin\\)\\s-?$")

(defconst ruby-electric-expandable-bar
  "\\s-\\(do\\|{\\)\\s-+|")

(defconst double-quoted-identifier-re
  "\"\\(\\w\\|_\\)+\"")

(defconst chars-preceding-quote-expand-re
  "\\s-\\|\\[\\|(\\|{\\|`\\|'")

(defconst chars-following-quote-expand-re
  "\\s-\\|\\]\\|)\\|}\\|`\\|'")

(defvar ruby-electric-matching-delimeter-alist
  '((?\[ . ?\])
    (?\( . ?\))
    (?\' . ?\')
    (?\` . ?\`)
    (?\" . ?\")))

(defcustom ruby-electric-simple-keywords-re
  "\\(def\\|if\\|class\\|module\\|unless\\|case\\|while\\|do\\|until\\|for\\|begin\\)"
  "*Regular expresion matching keywords for which closing 'end'
is to be inserted."
  :type 'regexp :group 'ruby-electric)

(defcustom ruby-electric-expand-delimiters-list '(all)
  "*List of contexts where matching delimiter should be
inserted. The word 'all' will do all insertions."
  :type '(set :extra-offset 8
              (const :tag "Everything" all )
              (const :tag "Curly brace" ?\{ )
              (const :tag "Square brace" ?\[ )
              (const :tag "Round brace" ?\( )
              (const :tag "Quote" ?\' )
              (const :tag "Double quote" ?\" )
              (const :tag "Back quote" ?\` )
              (const :tag "Vertical bar" ?\| ))
  :group 'ruby-electric)

(defcustom ruby-electric-newline-before-closing-bracket nil
  "*Controls whether a newline should be inserted before the
closing bracket or not."
  :type 'boolean :group 'ruby-electric)

(defcustom ruby-electric-insert-closing-curly-in-strings t
  "*Controls whether a closing curly brace ('}') should be inserted automatically
when inside a string."
  :type 'boolean :group 'ruby-electric)

(defcustom ruby-electric-insert-curlies-after-pound-in-strings t
  "*Controls whether a curly braces ('{}') should be inserted automatically
after a pound sign ('#') when inside a string."
  :type 'boolean :group 'ruby-electric)

(define-minor-mode ruby-electric-mode
  "Toggle Ruby Electric mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When Ruby Electric mode is enabled, an indented 'end' is
heuristicaly inserted whenever typing a word like 'module',
'class', 'def', 'if', 'unless', 'case', 'until', 'for', 'begin',
'do'. Simple, double and back quotes as well as braces are paired
auto-magically. Expansion does not occur inside comments and
strings. Note that you must have Font Lock enabled."
  ;; initial value.
  nil
  ;;indicator for the mode line.
  " REl"
  ;;keymap
  ruby-mode-map
  (ruby-electric-setup-keymap))

(defun ruby-electric-setup-keymap()
  (define-key ruby-mode-map " " 'ruby-electric-space)
  (define-key ruby-mode-map "\n" 'ruby-electric-newline)
  (define-key ruby-mode-map "{" 'ruby-electric-curlies)
  (define-key ruby-mode-map "(" 'ruby-electric-matching-char)
  (define-key ruby-mode-map "[" 'ruby-electric-open-matching-char)
  (define-key ruby-mode-map "\"" 'ruby-electric-quote)
  (define-key ruby-mode-map "\'" 'ruby-electric-quote)
  (define-key ruby-mode-map "\`" 'ruby-electric-matching-char)
  (define-key ruby-mode-map "|" 'ruby-electric-bar)
  (define-key ruby-mode-map "#" 'ruby-electric-pound)
  (define-key ruby-mode-map ":" 'ruby-electric-colon)

  (define-key ruby-mode-map "}" 'ruby-electric-close-curlies)
  (define-key ruby-mode-map ")" 'ruby-electric-close-matching-char)
  (define-key ruby-mode-map "]" 'ruby-electric-close-matching-char))

(defun ruby-electric-space (arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (ruby-electric-space-can-be-expanded-p)
      (save-excursion
        (ruby-indent-line t)
        (newline)
        (ruby-insert-end))))

(defun ruby-electric-newline (arg)
  (interactive "P")
  (newline-and-indent)
  (if (ruby-electric-newline-can-be-expanded-p)
      (save-excursion
        (ruby-indent-line t)
        (newline)
        (ruby-insert-end))))

(defun ruby-electric-code-at-point-p()
  (and ruby-electric-mode
       (let* ((properties (text-properties-at (point))))
         (and (null (memq 'font-lock-string-face properties))
              (null (memq 'font-lock-comment-face properties))))))

(defun ruby-electric-string-at-point-p()
  (and ruby-electric-mode
       (consp (memq 'font-lock-string-face (text-properties-at (point))))))

(defun ruby-electric-is-last-command-char-expandable-punct-p()
  (or (memq 'all ruby-electric-expand-delimiters-list)
      (memq last-command-char ruby-electric-expand-delimiters-list)))

(defun ruby-electric-space-can-be-expanded-p()
  (if (ruby-electric-code-at-point-p)
      (let* ((ruby-electric-keywords-re
              (concat ruby-electric-simple-keywords-re "\\s-$"))
             (ruby-electric-single-keyword-in-line-re
              (concat "\\s-*" ruby-electric-keywords-re)))
        (save-excursion
          (backward-word 1)
          (or (looking-at ruby-electric-expandable-do-or-begin-re)
              (progn
                (beginning-of-line)
                (looking-at ruby-electric-single-keyword-in-line-re)))))))

(defun ruby-electric-newline-can-be-expanded-p()
  (if (ruby-electric-code-at-point-p)
      (save-excursion
        (backward-word 1)
        (looking-at ruby-electric-expandable-do-or-begin-re))))

(defun ruby-electric-curlies(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (ruby-electric-is-last-command-char-expandable-punct-p)
      (cond ((ruby-electric-code-at-point-p)
             (cond (ruby-electric-newline-before-closing-bracket
                    (newline)
                    (insert "}")
                    (backward-char 1))
                   (t
                    (insert "  }")
                    (backward-char 2))))
            ((ruby-electric-string-at-point-p)
             (save-excursion
               (if ruby-electric-insert-closing-curly-in-strings
                   (insert "}")
                 (backward-char 1)
                 (when (char-equal ?\# (preceding-char))
                   (forward-char 1)
                   (insert "}"))))))))

(defun ruby-electric-close-curlies(arg)
  (interactive "P")
  (if (looking-at "}")
      (forward-char 1)
    (if (save-excursion
          (backward-char 2)
          (not (looking-at "{  }")))
        (self-insert-command (prefix-numeric-value arg))
      (backward-char 1)
      (delete-char 2)
      (forward-char 1))))

(defun ruby-electric-pound(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (when (and ruby-electric-insert-curlies-after-pound-in-strings
             (ruby-electric-string-at-point-p))
    (insert "{}")
    (backward-char 1)))

(defun ruby-electric-quote(arg)
  (interactive "P")
  (if (looking-at (string last-command-char))
      (forward-char 1)
    (if (looking-at ":")
        (progn (delete-char 1)
               (save-excursion
                 (self-insert-command (prefix-numeric-value arg))
                 (re-search-forward "[^ \t\n]*")
                 (self-insert-command (prefix-numeric-value arg))))
      ;; Require certain characters to precede and follow the quote in order to expand it
      (if (or
           (and (not (string-match chars-preceding-quote-expand-re (string (preceding-char))))
                (not (bolp)))
           (and (not (looking-at chars-following-quote-expand-re))
                (not (eolp))))
          (self-insert-command (prefix-numeric-value arg))
        (self-insert-command (prefix-numeric-value arg))
        (and (ruby-electric-is-last-command-char-expandable-punct-p)
             ;; The following line, if uncommented, prevents expansion in comments and strings.
             ;; (ruby-electric-code-at-point-p)
             (save-excursion
               (insert (cdr (assoc last-command-char
                                   ruby-electric-matching-delimeter-alist)))))))))

(defun ruby-electric-colon(arg)
  (interactive "P")
  (if (looking-at double-quoted-identifier-re)
      (save-excursion
        (delete-char 1)
        (insert ":")
        (re-search-forward "\"")
        (delete-backward-char 1))
    (self-insert-command (prefix-numeric-value arg))))

(defun ruby-electric-matching-char(arg)
  (interactive "P")
  (if (looking-at (string last-command-char))
      (forward-char 1)
    (self-insert-command (prefix-numeric-value arg))
    (and (ruby-electric-is-last-command-char-expandable-punct-p)
         ;; The following line, if uncommented, prevents expansion in comments and strings.
         ;(ruby-electric-code-at-point-p)
         (save-excursion
           (insert (cdr (assoc last-command-char
                               ruby-electric-matching-delimeter-alist)))))))

(defun ruby-electric-open-matching-char(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and (ruby-electric-is-last-command-char-expandable-punct-p)
       ;; The following line, if uncommented, prevents expansion in comments and strings.
       ;(ruby-electric-code-at-point-p)
       (save-excursion
         (insert (cdr (assoc last-command-char
                             ruby-electric-matching-delimeter-alist))))))

(defun ruby-electric-close-matching-char(arg)
  (interactive "P")
  (if (looking-at (string last-command-char))
      (forward-char 1)
    (self-insert-command (prefix-numeric-value arg))))

(defun ruby-electric-bar(arg)
  (interactive "P")
  (if (looking-at (string last-command-char))
      (forward-char 1)
    (self-insert-command (prefix-numeric-value arg))
    (and (ruby-electric-is-last-command-char-expandable-punct-p)
         (ruby-electric-code-at-point-p)
         (and (save-excursion (re-search-backward ruby-electric-expandable-bar nil t))
              (= (point) (match-end 0))) ;looking-back is missing on XEmacs
         (save-excursion
           (insert "|")))))

(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))



(provide 'ruby-electric)
