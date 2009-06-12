;;; tap-mode.el --- Major mode for editing .tap-files

;;; TAP is the Test Anything Protocol, a communication protocol
;;; between unit tests and a test harness.

;;; This mode mainly defines a grammar for syntax highlighting of TAP
;;; files.

;;; Copyright 2009 Steffen Schwigon

;;; Author: Steffen Schwigon <ss5@renormalist.net>
;;; Version: 0.02
;;; Keywords: testing tap
;;; X-URL: http://search.cpan.org/~schwigon/emacs-tap-mode/

;;; Tested on i386-linux with GNU Emacs 23.0.91.1.

;;; Commentary:

;;; This mode is built with help of the "Emacs language mode creation
;;; tutorial" written by Scott Andrew Borton, now hosted at
;;; 
;;;   http://renormalist.net/cgi-bin/twiki/view/Renormalist/EmacsLanguageModeCreationTutorial

;;; Usage:

;;; Put this file into your load-path and the following into your ~/.emacs:
;;;
;;;    (require 'tap-mode)
;;;
;;;
;;; To associate tap-mode with .tap files add the following to your ~/.emacs
;;;
;;;    (setq auto-mode-alist
;;;       (append auto-mode-alist
;;;         '(("tap" . tap-mode))))
;;;
;;;
;;; To automatically turn on font-lock-mode add the following to your ~/.emacs
;;;
;;;    (add-hook 'tap-mode-hook 'font-lock-mode)
;;;

;;; Code:

;; ---------- stolen from cperl-mode ----------  
(defconst tap-xemacs-p (string-match "XEmacs\\|Lucid" emacs-version))

(defun tap-choose-color (&rest list)
  (let (answer)
    (while list
      (or answer
	  (if (or (x-color-defined-p (car list))
		  (null (cdr list)))
	      (setq answer (car list))))
      (setq list (cdr list)))
    answer))

;; create and activate syntax table
(defun tap-create-syntax-table ()
  (if tap-mode-syntax-table
      ()
    (setq tap-mode-syntax-table (make-syntax-table))
    (set-syntax-table tap-mode-syntax-table)
    ))

(defgroup tap nil
  "Major mode for TAP (Test Anything Protocol)."
  :prefix "tap-"
  :group 'languages)

(defgroup tap-faces nil
  "Fontification colors."
  :prefix "tap-"
  :group 'tap)

(defvar tap-can-font-lock
  (or tap-xemacs-p
      (and (boundp 'emacs-major-version)
           (or window-system
               (> emacs-major-version 20)))))

(if tap-can-font-lock
    (progn
      (defvar tap-dark-foreground
        (tap-choose-color "orchid1" "orange"))

      (defface tap-version-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "green4"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-version-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "green4" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-version-number-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "green4" :bold t))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-version-number-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "green4" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-plan-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "blue3"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-plan-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "blue3" :weight bold :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-plan-tests-planned-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "blue4" :weight bold))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-plan-tests-planned-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "blue4" :weight bold :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-plan-directive-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "lightsteelblue3"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-plan-directive-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "lightsteelblue3" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-plan-directive-explanation-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "purple3"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-plan-directive-explanation-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "purple3" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-test-ok-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "red4"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-test-ok-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "red4" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-test-notok-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "red4" :bold t))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-test-notok-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "red4" :bold t :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-test-num-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "blue"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold nil))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-test-num-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "blue" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold nil))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-test-description-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "orange4"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-test-description-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "orange4" :weight bold :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-test-directive-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "lightsteelblue3"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-test-directive-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "lightsteelblue3" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-test-unknown-directive-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "orange3"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-test-unknown-directive-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "orange3" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-test-directive-explanation-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "purple3"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-test-directive-explanation-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "purple3" :weight bold :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-pragma-keyword-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "green3"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-pragma-keyword-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "green3" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold t))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-pragma-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "green4" :bold t))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold nil))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-pragma-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "green4" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold nil))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-comment-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "steelblue3"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold nil))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      (defface tap-nested-comment-face
        (` ((((class grayscale) (background light))
             (:background "Gray90" :italic t))
            (((class grayscale) (background dark))
             (:foreground "Gray80" :italic t :bold t))
            (((class color) (background light))
             (:foreground "steelblue3" :background "Gray91"))
            (((class color) (background dark))
             (:foreground (, tap-dark-foreground)))
            (t (:bold nil))))
        "Font Lock mode face used to highlight array names."
        :group 'tap-faces)

      ))

;; default variables
(defvar tap-mode-hook nil)

;; keymap
(defvar tap-mode-map nil "Keymap for TAP major mode.")
(if tap-mode-map nil
  (let ((map (make-sparse-keymap)))
    ;; insert (define-key map ...) stuff here
    (setq tap-mode-map map)))

;; syntax highlighting: standard keywords
(defconst tap-font-lock-keywords-1
  '(
    )
  "Minimal highlighting expressions for TAP mode.")

;; syntax highlighting: additional keywords
(defconst tap-font-lock-keywords-2
  (append tap-font-lock-keywords-1
	  '(
	    ))
  "Additional Keywords to highlight in TAP mode.")

;; syntax highlighting: even more keywords
(defconst tap-font-lock-keywords-3
  (append tap-font-lock-keywords-2
	  '(
            ;; ----- non-nested TAP -----
            ;; version
            ("^\\(TAP version +\\)\\([0-9]+\\)" 
             (1 'tap-version-face)
             (2 'tap-version-number-face)
             )
            ;; simple plan
            ("^\\(1\\.\\.\\)\\([0-9]+\\)"
             (1 'tap-plan-face)
             (2 'tap-plan-tests-planned-face)
             )
            ;; extended plan
            ("^\\(1\\.\\.\\)\\([0-9]+\\)\\( *# *\\(todo\\|skip\\)\\>\\)\\(.*\\)"
             (1 'tap-plan-face)
             (2 'tap-plan-tests-planned-face)
             (4 'tap-plan-directive-face)
             (5 'tap-plan-directive-explanation-face)
             )
            ;; test lines with only directives
            ("^\\(\\(not \\)?\\(ok\\)\\)\\> *\\([0-9]*\\) *# *\\(todo\\|skip\\)\\> *\\(.*\\)"
             (1 'tap-test-notok-face)
             (3 'tap-test-ok-face)
             (4 'tap-test-num-face)
             (5 'tap-test-directive-face)
             (6 'tap-test-directive-explanation-face)
             )
            ;; test lines with description and directive
            ("^\\(\\(not \\)?\\(ok\\)\\)\\> *\\([0-9]*\\) *\\([^#[:cntrl:]]+\\) +# *\\(todo\\|skip\\)\\> *\\(.*\\)"
             (1 'tap-test-notok-face)
             (3 'tap-test-ok-face)
             (4 'tap-test-num-face)
             (5 'tap-test-description-face)
             (6 'tap-test-directive-face)
             (7 'tap-test-directive-explanation-face)
             )
            ;; test lines with only description
            ("^\\(\\(not \\)?\\(ok\\)\\)\\> *\\([0-9]*\\) *\\([^#[:cntrl:]]*\\)"
             (1 'tap-test-notok-face)
             (3 'tap-test-ok-face)
             (4 'tap-test-num-face)
             (5 'tap-test-description-face)
             )
            ;; pragmas
            ("^\\(pragma\\) +\\(.+\\)"
             (1 'tap-pragma-keyword-face)
             (2 'tap-pragma-face)
             )
            ;; comments
            ("^\\(#.*\\)$"
             (1 'tap-comment-face)
             )

            ;; ----- nested TAP -----
            ;; version
            ("^ +\\(TAP version +\\)\\([0-9]+\\)" 
             (1 'tap-nested-version-face)
             (2 'tap-nested-version-number-face)
             )
            ;; simple plan
            ("^ +\\(1\\.\\.\\)\\([0-9]+\\)"
             (1 'tap-nested-plan-face)
             (2 'tap-nested-plan-tests-planned-face)
             )
            ;; extended plan
            ("^ +\\(1\\.\\.\\)\\([0-9]+\\)\\( *# *\\(todo\\|skip\\)\\>\\)\\(.*\\)"
             (1 'tap-nested-plan-face)
             (2 'tap-nested-plan-tests-planned-face)
             (4 'tap-nested-plan-directive-face)
             (5 'tap-nested-plan-directive-explanation-face)
             )
            ;; test lines with only directives
            ("^ +\\(\\(not \\)?\\(ok\\)\\)\\> *\\([0-9]*\\) *# *\\(todo\\|skip\\)\\> *\\(.*\\)"
             (1 'tap-nested-test-notok-face)
             (3 'tap-nested-test-ok-face)
             (4 'tap-nested-test-num-face)
             (5 'tap-nested-test-directive-face)
             (6 'tap-nested-test-directive-explanation-face)
             )
            ;; test lines with description and directive
            ("^ +\\(\\(not \\)?\\(ok\\)\\)\\> *\\([0-9]*\\) *\\([^#[:cntrl:]]+\\) +# *\\(todo\\|skip\\)\\> *\\(.*\\)"
             (1 'tap-nested-test-notok-face)
             (3 'tap-nested-test-ok-face)
             (4 'tap-nested-test-num-face)
             (5 'tap-nested-test-description-face)
             (6 'tap-nested-test-directive-face)
             (7 'tap-nested-test-directive-explanation-face)
             )
            ;; test lines with only description
            ("^ +\\(\\(not \\)?\\(ok\\)\\)\\> *\\([0-9]*\\) *\\([^#[:cntrl:]]*\\)"
             (1 'tap-nested-test-notok-face)
             (3 'tap-nested-test-ok-face)
             (4 'tap-nested-test-num-face)
             (5 'tap-nested-test-description-face)
             )
            ;; pragmas
            ("^ +\\(pragma\\) +\\(.+\\)"
             (1 'tap-nested-pragma-keyword-face)
             (2 'tap-nested-pragma-face)
             )
            ;; comments
            ("^ +\\(#.*\\)$"
             (1 'tap-nested-comment-face)
             )

            ))
  "Balls-out highlighting in TAP mode.")

;; default level of highlight to maximum
(defvar tap-font-lock-keywords tap-font-lock-keywords-3
  "Default highlighting expressions for TAP mode")

;; no special indenting, just pure text mode
(defun tap-indent-line ()
  "Indent current line as TAP code. Does nothing in this mode."
  (interactive)
  )

;; no special syntax table
(defvar tap-mode-syntax-table nil
  "Syntax table for tap-mode.")

;; main
(defun tap-mode ()
  "Major mode for editing TAP files (Plain Old Documentation for Perl)."
  (interactive)
  (kill-all-local-variables)
  (tap-create-syntax-table)
  (use-local-map tap-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((tap-font-lock-keywords-1
                              tap-font-lock-keywords-2
                              tap-font-lock-keywords-3)
                             't ;; KEYWORDS-ONLY
                             't ;; CASE-FOLD
                             ))
  (setq major-mode 'tap-mode)
  (setq mode-name "TAP")
  (setq imenu-generic-expression '((nil "^not ok \\(.*\\)" 1)))
  (run-hooks 'tap-mode-hook))

(provide 'tap-mode)

;;; tap-mode.el ends here
