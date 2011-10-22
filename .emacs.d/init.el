;;;; -*- Emacs-Lisp -*- GNU Emacs Start-Up options

(defvar emacslib "~/.emacs.d/cesar/"
  "Pathname of my private library")
(setq load-path (append (list emacslib) load-path (list ".")))

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq make-backup-files nil)

(require 'server)
(server-start)

(put 'eval-expression  'disabled	nil)
(put 'narrow-to-region 'disabled	nil)
(put 'narrow-to-page   'disabled	nil)
(put 'set-goal-column  'disabled	nil)

(setq font-lock-maximum-size		1000000)
(setq font-lock-maximum-decoration	t)
(setq gc-cons-threshold			500000)
(setq inhibit-startup-message		t)
(setq kept-new-versions			3)
(setq kept-old-versions			1)
(setq list-directory-brief-switches	"-aCF")
(setq list-directory-verbose-switches	"-algF")
(setq minibuffer-max-depth		nil)
(setq next-line-add-newlines		nil)
(setq require-final-newline		t)
(setq scroll-step			1)
(setq search-slow-window-lines		3)
(setq shell-file-name			"/bin/bash")
(setq trim-versions-without-asking	t)
(setq version-control			t)
(setq visible-bell			t)

(setq-default case-fold-search		t)
(setq-default case-replace		t)
(setq-default compile-command		"time -p make -j8 ")
(setq-default diff-switches		"-u")
(setq-default indent-tabs-mode		t)
(setq-default fill-column		78)

;;; Mode Hooks

;;(add-hook 'c-mode-hook			'cq-c-mode)
;;(add-hook 'c++-mode-hook		'cq-c++-mode)
;;(add-hook 'java-mode-hook		'cq-java-mode)
(setq c-style-variables-are-local-p 	t)

;;(require 'xcscope)
;;(setq cscope-program "mlcscope")

(autoload 'set-balanced-insertions "balanced-insertions" t nil)
(autoload 'cq-buffer-menu "cq-buffer-menu-mode" t nil)
(autoload 'cq-insert-braces "cq-c-mode" t nil)
(autoload 'cq-backward-up-list-or-string "cq-c-mode" t nil)
(autoload 'cq-backward-down-list "cq-c-mode" t nil)

(add-hook 'awk-mode-hook
	  (lambda ()
            (common-cc-mode-hook-preamble)
            (setq c-basic-offset 2)
	    (auto-fill-mode 1)
            (hs-minor-mode 1)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))))

(add-hook 'c-mode-common-hook
	  (lambda ()
            (set-balanced-insertions)
            (local-set-key "\C-z{"   'cq-insert-braces)
            (local-set-key "\C-\M-u" 'cq-backward-up-list-or-string)
            (local-set-key "\C-\M-d" 'cq-down-list)	    (setq c-basic-offset 2)
	    (c-set-offset 'substatement-open 0)
	    (c-set-offset 'inextern-lang 0)
	    (c-set-offset 'inline-open 0)
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)
	    ;; (local-set-key "\M-\C-a" 'c-beginning-of-defun)
	    ;; (local-set-key "\M-\C-e" 'c-end-of-defun)
            (hs-minor-mode 1)
            (if (string-match "^GNU Emacs" (emacs-version))
                (subword-mode 1)
              (c-subword-mode 1))
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))))

(add-hook 'comint-mode-hook
	  (lambda ( )
	    (add-hook 'comint-output-filter-functions
		      'comint-strip-ctrl-m)))

(add-hook 'compilation-mode-hook
	  (lambda ()
            (font-lock-mode 1)))

(add-hook 'cperl-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
            (hs-minor-mode 1)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))
	    (setq cperl-tab-always-indent nil)))

(add-hook 'diff-mode-hook
	  (lambda ()
	    (font-lock-mode 1)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (setq fill-column 79)
	    (setq indent-tabs-mode nil)
            (hs-minor-mode 1)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))
	    (require 'balanced-insertions)
	    (set-balanced-insertions )
	    (local-set-key "\C-zi"    'cq-both-in)
	    (local-set-key "\C-zo"    'cq-both-out)
	    (local-set-key "\C-zx"    'cq-extract-sexp)
	    (local-set-key "\C-\M-u"  'cq-backward-up-list-or-string)
	    (local-set-key "\C-\M-d"  'cq-down-list)
	    ))

(setq inferior-lisp-program "clisp")
(add-hook 'ilisp-mode-hook
	  (lambda () (setq lisp-no-popper t)))
(add-hook 'lisp-mode-hook		;tailored for CLISP-HS
	  (lambda ()
	    ;; (require 'ilisp)
            ;; (require 'slime)
	    (require 'balanced-insertions)
	    (set-balanced-insertions )
	    (local-set-key "\C-zi"    'cq-both-in)
	    (local-set-key "\C-zo"    'cq-both-out)
	    (local-set-key "\C-zx"    'cq-extract-sexp)
	    (local-set-key "\C-\M-u"  'cq-backward-up-list-or-string)
	    (local-set-key "\C-\M-d"  'cq-down-list)
	    ;;(local-set-key "\C-z]"    'cq-close-defun)
	    (auto-fill-mode 1)
            (hs-minor-mode 1)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))
	    (setq lisp-no-popper t)	;this is smoother in Cygwin
	    ;;(put 'if 'lisp-indent-function nil)
	    (let* ((name (buffer-file-name (current-buffer)))
		   (ext (and name (file-name-extension name))))
	      (if (and ext
		       (or (string= ext "emacs")
			   (string= ext "el")
			   (string= ext "elc")))
		  nil			;must be Emacs Lisp
		(make-local-variable 'lisp-indent-function)
		(setq lisp-indent-function 'common-lisp-indent-function))
	      (put 'defclass 'lisp-indent-function 3)
	      (put 'defgeneric 'lisp-indent-function 'defun)
	      ;; (put 'defmethod 'lisp-indent-function 'defun)
	      (put 'define-method-combination 'lisp-indent-function 2)
	      (put 'generic-flet 'lisp-indent-function
		   (get 'flet 'lisp-indent-function))
	      (put 'generic-labels 'lisp-indent-function
		   (get 'labels 'lisp-indent-function))
	      (put 'generic-function 'lisp-indent-function
		   (get 'lambda 'lisp-indent-function))
	      (put 'with-accessors 'lisp-indent-function 2)
	      (put 'with-slots 'lisp-indent-function 2)
	      (put 'with-added-methods 'lisp-indent-function 'defun))))
(setq common-lisp-hyperspec-root
      "http://www.lispworks.com/documentation/HyperSpec/")

(add-hook 'makefile-mode-hook
	  (lambda ()
	    (auto-fill-mode 0)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))))

(add-hook 'pascal-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))
	    (setq pascal-tab-always-indent nil)))

(add-hook 'shell-mode-hook
	  (lambda ()
	    (auto-fill-mode 0)
	    (lsetq shell-pushd-regexp "pushd\\|\\+")
	    (lsetq shell-popd-regexp "popd\\|-")
	    (setq shell-prompt-pattern "^[$%#] "
                  comint-prompt-regexp shell-prompt-pattern)))

(add-hook 'shell-mode-hook
          (lambda ()
            (make-local-variable 'mode-line-format)
            (setq mode-line-format
                  (list " %b(%s) %p (%l,%c) "
                        'default-directory
                        " "
                        'global-mode-string
                        "("
                        'mode-name
                        'minor-mode-alist
                        ") %-")))
          t)

(add-hook 'sh-mode-hook
	  (lambda ()
	    (auto-fill-mode 0)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))))

(add-hook 'tcl-mode-hook
	  (lambda ()
	    (setq tcl-tab-always-indent nil)
	    (tcl-auto-fill-mode 1)
            (hs-minor-mode 1)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))))
(setq tcl-application "tclsh"
      tcl-prompt-regexp "\(^> \)|\(^\\ \)")

(add-hook 'vhdl-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))))

(add-hook 'xml-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))))

(add-hook 'xrdb-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (font-lock-mode (and (< (buffer-size) font-lock-maximum-size) 1))))

;;; For FORTRAN mode
(add-hook 'fortran-mode-hook             'cq-fortran-mode)

;;; For Mod2 mode
(add-hook 'mod2-mode-hook                'cq-mod2-mode)

;;; For Yacc mode
(add-hook 'yacc-mode-hook                'cq-yacc-mode)

;;; For Text modes
(add-hook 'text-mode-hook  	            'cq-text-mode)
(add-hook 'nroff-mode-hook 	            'cq-nroff-mode)
(add-hook 'edit-picture-hook             'cq-picture-mode)
(add-hook 'texinfo-mode-hook	            'cq-texinfo-mode)
(add-hook 'TeX-mode-hook   	            'cq-tex-mode)
(add-hook 'LaTeX-mode-hook 	            'cq-latex-mode)
(setq TeX-default-mode 	            'latex-mode)

;;; For the Babyl mailer
;(setq mail-archive-file-name        "~/mail/babyl/OUT")
;(setq mail-self-blind               nil)
;(setq mail-use-rfc822               t)
;(setq rmail-delete-after-output     nil)
;(setq rmail-file-name               "~/mail/babyl/RMAIL")

;;; For Prolog mode
(add-hook 'prolog-mode-hook              'cq-prolog-mode)

;;; Dired Extensions
(add-hook 'dired-mode-hook               'cq-dired-mode)

;;; Buffer-Menu Extensions
(add-hook 'buffer-menu-mode-hook         'cq-buffer-menu)

;;; Reingold's calendar and diary program
;;; The idea is to share the screen with MH-E on startup,
;;; as I start things usually this way:
;;;     emacs -f display-time -f cq-inbox -f calendar

;;; First off, don't spend too much time on this.
(setq view-diary-entries-initially      nil)
(setq mark-holidays-in-calendar         nil)
(setq mark-diary-entries-in-calendar    nil)
(setq diary-file                        "~/.diary")
(setq european-calendar-style           nil)
(setq calendar-date-display-form	'((format "%s-%02s-%02s"
                                                  year month day)))
(setq calendar-week-start-day		1)
(autoload 'calendar "calendar" "" t nil)
(autoload 'diary    "diary"    "" t nil)
(autoload 'holidays "holidays" "" t nil)
(add-hook 'initial-calendar-window-hook
          (function (lambda ()
                      (when (and ecb-minor-mode
                                 (ecb-point-in-edit-window-number))
                        ;; if no horizontal split then nothing
                        ;; special to do
                        (or (= (frame-width) (window-width))
                            (shrink-window (- (window-height) 9))))
                      )))

;;; Other customizations
(setq safe-kill-region-threshold        5120)
(setq lpq-known-printers                '())
(setq set-mark-command-repeat-pop t)


;;; Other hooks

(add-hook 'after-save-hook
	  (lambda ()
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(and (looking-at "^#!")	;magic number detector
		     (not (file-executable-p buffer-file-truename))
		     (set-file-modes buffer-file-truename
				     (logior (file-modes buffer-file-truename)
					     #o755)))))))

;;;
;;; Some global key bindings
;;;

;;;(global-unset-key "\C-x\C-l")           ;was downcase-region
;;;(global-unset-key "\C-x\C-u")           ;was upcase-region

(global-unset-key "\C-z")               ;make it user-specific (thanks Neil)

(global-set-key [(button4)]		'scroll-down-one)
(global-set-key [(button5)]		'scroll-up-one)
(global-set-key [(control button4)] 	'backward-block-of-lines)
(global-set-key [(control button5)]	'forward-block-of-lines)
(global-set-key [(meta button4)]	'scroll-down-command)
(global-set-key [(meta button5)]	'scroll-up-command)

(global-set-key "\C-^"              'call-last-kbd-macro)
(global-set-key "\ew"               'copy-region-as-kill-command)
(global-set-key [ ?\C-c ?\  ]       'set-mark-command)
(global-set-key "\^Z\^C"            'compile)
(global-set-key "\^Z\^K"            'kill-compilation)
(global-set-key "\^Z\^N"            'next-error)
;(global-set-key "\^X\^C"            'cq-kill-emacs-maybe)
(global-set-key "\^X="              'cq-what-cursor-position)
(global-set-key "\^X\^B"            'buffer-menu);was electric-buffer-list
(global-set-key "\^Zv"              'shrink-window)
(global-set-key "\^X\""             'double-line)
(global-set-key "\^X\^L"            'inferior-lisp)
(global-set-key "\^X\^N"	    'set-goal-column)
(global-set-key "\^X4!"             'cq-multi-shell-other-window)
(global-set-key "\^X!"              'cq-multi-shell)
(global-set-key "\^W"               'safe-kill-region)
(global-set-key "\^Z\^F"            'cq-revert-buffer)
(global-set-key "\^Zb"		    'cq-insert-buffer-file-name)
(global-set-key "\^Zg"              'cq-goto-line)
(global-set-key "\^Zt"              'cq-timestamp)
(global-set-key "\^Zm"		    'vm-compose-mail)
(global-set-key "\^Zr"		    'vm)

;; (define-key help-map "\C-f"         'flash-describe-function)
;; (define-key help-map "\C-v"         'flash-describe-variable)

;;; For electric-minibuffer
(define-key minibuffer-local-completion-map "\en"
  'minibuffer-yank-next-completion)
(define-key minibuffer-local-completion-map "\ep"
  'minibuffer-yank-previous-completion)
(define-key minibuffer-local-must-match-map "\en"
  'minibuffer-yank-next-completion)
(define-key minibuffer-local-must-match-map "\ep"
  'minibuffer-yank-previous-completion)

;;; Load Abbreviations.
(quietly-read-abbrev-file nil)

;;; Functions to load on demand.

(let ((file (concat emacslib "cq-c-mode")))
  (autoload 'cq-c-mode file "C mode settings" t nil)
  (autoload 'cq-c++-mode file "C++ mode settings" t nil)
  (autoload 'cq-java-mode file "Java mode settings" t nil)
  (autoload 'emacs-c-source-mode file "Use Emacs's C style." t nil)
  (autoload 'c-main    file "C main program builder" t nil)
  (autoload 'c-non-main file "C headers/library builder" t nil)
  (autoload 'c-extern-trick file "Disgusting hacks for .h externs" t nil)
  (autoload 'c68-catch file "Build a catch block" t nil)
  (autoload 'c68-when  file "Build a when clause" t nil)
  (autoload 'cq-insert-braces file "Insert a pair of matching braces." t nil))

(let ((file (concat emacslib "cq-lisp-mode")))
  (autoload 'cq-lisp-mode file "Lisp Mode customizations" t nil)
  (autoload 'cq-emacs-lisp-mode file "Emacs-Lisp Mode customizations" t nil)
  (autoload 'cq-lisp-interaction-mode file "Lisp Interaction details" t nil)
  (autoload 'cq-common-lisp-mode file "Common Lisp interaction" t nil)
  (autoload 'resume-lisp file "Run or restart inferior lisp" t nil)
  (autoload 'resume-scheme file "Run or restart inferior scheme" t nil)
  ;;; structured editing
  (autoload 'cq-both-in file "Parenthesize the sexps in the region" t nil)
  (autoload 'cq-both-out file "Un-parenthesize the sexps in the region" t nil)
  (autoload 'cq-extract-sexp file
    "Sexp at or after point replaces its whole list" t nil)
  (autoload 'cq-close-defun file
    "Put enough delimiters to close the current defun" t nil)
  ;;; structured in-file navigation
  (autoload 'cq-backward-up-list file "Like \\C-M-u, strings are safe" t nil)
  (autoload 'cq-backward-up-list-or-string file
            "Exits up from lists or strings" t nil)
  (autoload 'cq-down-list file "Like \\C-M-d, doesn't fail on strings" t nil))



(let ((file (concat emacslib "cq-utils")))
  (autoload 'double-line file "neato" t nil)
  (autoload 'cq-suspend-saving file "Suspend and save all buffers" t nil)
  (autoload 'cq-iconify-saving file "Iconify and save all buffers" t nil)
  (autoload 'cq-goto-match file "Take cursor to matching delimiter" t nil)
  (autoload 'x-mouse-chart file "Reminder of X-mouse bindings" t nil)
  (autoload 'x-reconfig    file "Reconfigure X window." t nil)
  (autoload 'x-mouse-select-and-mark file
            "Select window, set mark where cursor landed" t nil)
  (autoload 'cq-no-suspend file "Do not suspend this EMACS" t nil)
  (autoload 'cq-kill-emacs-maybe file "Do not kill this EMACS recklessly"
            t nil)
  (autoload 'cq-shell file "Start/Restart a shell" t nil)
  (autoload 'cq-shell-other-window file "Shell in another window." t nil)
  (autoload 'cq-comint-shell-other-window file
    "Shell in another window." t nil)
  (autoload 'find-file-read-only-other-window file "Like ^X^R" t nil)
  (autoload 'tags-find-all file "Find all file in tags file." t nil)
  (autoload 'cq-what-cursor-position file "Report cursor position." t nil)
  (autoload 'set-flow-control file "Set/Reset flow controlled input." t nil)
  (autoload 'cq-switch-del-and-bs file "BS<->DEL" t nil)
  (autoload 'cq-save-modified-buffers file "Saved all modified buffers."
            t nil)
  (autoload 'see-chars file
            "Displays characters typed, terminated by a 3-second timeout."
            t nil)
  (autoload 'first-window file
            "Other-window implemented to be used from the command line."
            t nil)
  (autoload 'identify-in-minibuffer file
            "Verbose identification dump."
            t nil)
  (autoload 'safe-kill-region file
            "Kill region, ask for confirmation if too large." t nil)
  (autoload 'flash-describe-function file
            "Flash the documentation of FUNCTION (a symbol)" t nil)
  (autoload 'flash-describe-variable file
            "Flash the documentation of VARIABLE (a symbol)" t nil)
  (autoload 'cq-display-temporarily file
            "Temporary display of a string, if doable" t nil)
  (autoload 'cq-revert-buffer file "Like revert-buffer, but less chatty" t nil)
  (autoload 'copy-region-as-kill-command file
            "Chatty copy-region-as-kill" t nil)
  (autoload 'attach-to-register file
            "Either prepend or append to register" t nil)
  (autoload 'force-recenter file "(recenter '(0))" t nil)
  (autoload 'cq-goto-line file "Go to line given by the prefix, a la CCA"
            t nil)
  (autoload 'cq-timestamp file "Insert a time stamp at point" t nil)
  (autoload 'lsetq file "Setq variables after making them buffer-local" nil t)
  (autoload 'locate-indentation file "Find indentation extent for current line."
    nil t ))

(let ((file (concat emacslib "cq-dired-mode")))
  (autoload 'cq-dired-mode file "Customizations of DIRED mode" t nil))

(let ((file (concat emacslib "electric-minibuffer")))
  (autoload 'minibuffer-yank-next-completion file "M-n" t nil)
  (autoload 'minibuffer-yank-previous-completion file "M-p" t nil))

(let ((file (concat emacslib "starweb")))
  (autoload 'starweb-mode file "Supports writing orders" t nil))

(defun wrap-up-start ()
  (setq display-time-day-and-date t
        display-time-24hr-format  t
        display-time-format       "%Y-%m-%d %H:%M ")
  (display-time)
  (message "%s (%s) at %s:%s (%s).  Hi!"
           (user-full-name) (user-login-name)
           (getenv "HOST") (getenv "TTY") (getenv "TERM")))

;;;end .emacs-shared, loaded from ~/.emacs.d/init.el, ~/.xemacs/init.el


(load-library "cl")
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(setq load-path (cons "~/.emacs.d/cesar" load-path))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/Users/jack/.emacs.d/lisp/ac-dict")
(ac-config-default)

(require 'template)
(template-initialize)

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(require 'ruby-electric)

(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))

(cond ((and (string-match "^GNU Emacs" (emacs-version))
            (>= emacs-major-version 21)
            window-system)
       (if (>= emacs-major-version 22)
           (setq font-lock-support-mode 'jit-lock-mode)
         (setq font-lock-support-mode 'lazy-lock-mode))
       (when (>= emacs-major-version 21)
         (defun zap-up-to-char (arg char)
           "Kill up to, but not including, ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
           (interactive "p\ncZap to char: ")
           (kill-region (point) (progn
                                  (search-forward (char-to-string char) nil nil arg)
                                  (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
                                  ))))
       (setq show-paren-style 'expression)

       ;;(setq load-path (cons "~/Downloads/shared-emacs-pkgs/slime" load-path))
(let* ((pkgs-basedir "~/emacs_packages/")
              (cedet-common-lib (concat pkgs-basedir "cedet/common/cedet")))
	 (setq load-path (cons (concat pkgs-basedir "cedet") load-path))
	 (setq load-path (cons (concat pkgs-basedir "ecb") load-path))
	 (setq load-path (cons (concat pkgs-basedir "parenface") load-path))
	 (setq load-path (cons (concat pkgs-basedir "org/lisp") load-path))
	 (setq load-path (cons (concat pkgs-basedir "remember") load-path))

	 (load-library cedet-common-lib))

       (require 'gamegrid)
       (defun gamegrid-add-score-with-update-game-score-1( file target score ))

       (require 'uniquify)

       (add-hook 'dired-load-hook
                 (lambda ()
                   ;; Bind dired-x-find-file.
                   (setq dired-x-hands-off-my-keys nil)
                   (load "dired-x")
                   ))
       (require 'dired-x)

       ;; (require 'slime)
       ;; (slime-setup)

       (require 'org-install)
       (add-to-list 'auto-mode-alist (cons "\\.org$" 'org-mode))
       (global-set-key "\C-cl" 'org-store-link)
       (global-set-key "\C-ca" 'org-agenda)
       (global-set-key "\C-cb" 'org-iswitchb)
       (setq org-todo-keywords
             '((sequence "TODO" "IN PROGRESS" "DONE" "DROPPED")))
       (setq org-log-done t)            ; or '(done) instead of t
       (setq org-agenda-include-diary t)
       (add-hook 'org-mode-hook (lambda () (require 'vc)))
       (add-hook 'org-mode-hook 'turn-on-font-lock)

       (require 'remember)
       (org-remember-insinuate)
       (setq org-directory "~/notes")
       (setq org-default-notes-file (concat org-directory "/notes.org"))
       (define-key global-map "\C-cr" 'org-remember)

       (setq semantic-load-turn-everything-on t)
       (require 'semantic-load)
       (global-ede-mode 1)
       (semantic-load-enable-code-helpers)
       ;; (semantic-load-enable-gaudy-code-helpers)
       ;; (semantic-load-enable-excessive-code-helpers)
       (global-srecode-minor-mode 1)
       (require 'ecb)
       (require 'cq-x-utils)
       (require 'font-lock)
       (require 'parenface)
       (require 'linum+)
       (setq linum-format 'dynamic)
       (global-linum-mode 1)
       (fringe-mode 0)

       (setq load-path (cons "~/.emacs.d/color-theme" load-path))
       (setq load-path (cons "~/.emacs.d/color-theme-solarized" load-path))
       (require 'color-theme)
       (require 'color-theme-solarized)

       ;; compensate for the frame-background-mode setting
       (defun set-color-theme-solarized (mode)
         "Switch to solarized-MODE, adjusting the frame-background-mode too."
         (interactive "Slight or dark? ")
         (unless (memq mode '(light dark))
           (error "Bad mode '%s, should be one of 'light or 'dark" mode))
         (let ((sym 'frame-background-mode)
               (frame (selected-frame))
               (rows (frame-height))
               (cols (frame-width)))
           (message "Frame geometry before: %dx%d" cols rows)
           (funcall (or (get sym 'custom-set) 'set) sym mode)
           (color-theme-solarized mode)
           (adjust-paren-face-fg)
           (set-frame-size frame cols rows)))

       (defun set-color-theme-solarized-light ()
         "Convenience invocation for (set-color-theme-solarized 'light)"
         (interactive)
         (set-color-theme-solarized 'light))

       (defun set-color-theme-solarized-dark ()
         "Convenience invocation for (set-color-theme-solarized 'dark)"
         (interactive)
         (set-color-theme-solarized 'dark))

       (defun set-color-theme-solarized-flip ()
         "Flip the background mode (from light or default to dark, and from dark to light."
         (interactive)
         (let* ((ofbm frame-background-mode)
                (nfbm (case ofbm
                        (light 'dark)
                        (dark 'light)
                        (otherwise 'dark))))
           (set-color-theme-solarized nfbm)))

       (fset 'dark (symbol-function 'set-color-theme-solarized-dark))
       (fset 'light (symbol-function 'set-color-theme-solarized-light))

       (defvar menu-bar-solarized-menu (make-sparse-keymap "LIght or Dark?"))
       (define-key menu-bar-solarized-menu [solarized-light]
         '(menu-item "Light mode" set-color-theme-solarized-light
                     :help "Set color theme to solarized, with light background"))
       (define-key menu-bar-solarized-menu [solarized-dark]
         '(menu-item "Dark mode" set-color-theme-solarized-dark
                     :help "Set color theme to solarized, with dark background"))
       (define-key menu-bar-solarized-menu [solarized-flip]
         '(menu-item "Flip bg mode" set-color-theme-solarized-flip
                     :help "Flip background mode between light and dark"))
       (define-key menu-bar-options-menu [color-theme-background]
         (list 'menu-item "Light or Dark?" menu-bar-solarized-menu))

       (global-set-key [f12] 'set-color-theme-solarized-flip)))

(defvar *workaround*frame-height* 40)
(defvar *workaround*frame-width* 90)
(global-set-key [(meta z)] 'zap-up-to-char)
(global-set-key [(meta Z)] 'zap-to-char)

(wrap-up-start)
(when (memq window-system (list 'x 'w32))
       (set-color-theme-solarized-light)
       (set-default-xtitle))

;;;end ~/.emacs -- don't edit beyond

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(adaptive-fill-mode t)
 '(align-indent-before-aligning t)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compile-command "time -p make -j8 ")
 '(current-language-environment "Latin-1")
 '(default-frame-alist (quote ((width . 90) (height . 40) (menu-bar-lines . 1) (tool-bar-lines . 0))))
 '(default-input-method "latin-9-prefix")
 '(dired-dwim-target t)
 '(dired-isearch-filenames (quote dwim))
 '(display-time-mode t)
 '(ecb-auto-expand-tag-tree (quote all))
 '(ecb-layout-name "left5")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote (("/Users/jack" "Home") ("/" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-buffer-style (quote image))
 '(ecb-tree-expand-symbol-before t)
 '(ecb-tree-indent 0)
 '(ecb-tree-stickynode-indent-string "")
 '(ediff-custom-diff-options "-U10")
 '(ediff-prefer-iconified-control-frame t)
 '(ediff-show-clashes-only t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-use-toolbar-p nil)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enable-recursive-minibuffers t)
 '(gdb-enable-debug t)
 '(gdb-max-frames 64)
 '(gdb-use-separate-io-buffer t)
 '(global-font-lock-mode t nil (font-lock))
 '(grep-command "grep -n -e ")
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(mouse-autoselect-window -0.25)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-yank-at-point t)
 '(normal-erase-is-backspace t)
 '(org-startup-indented t)
 '(pc-selection-mode t nil (pc-select))
 '(recentf-mode t)
 '(require-final-newline nil)
 '(scroll-bar-mode (quote right))
 '(shell-popd-regexp "popd\\|-")
 '(shell-pushd-regexp "pushd\\|+")
 '(show-trailing-whitespace t)
 '(tab-always-indent (quote complete))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil nil (tool-bar))
 '(truncate-lines nil)
 '(use-file-dialog nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
