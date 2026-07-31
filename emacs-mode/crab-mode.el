;;; crab-mode.el --- Major mode for CrabIR, the IR printed by Clam's --ocrab -*- lexical-binding: t; -*-

;; Keywords: languages, crab, crabir, clam
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Major mode for viewing CrabIR.
;;
;; IMPORTANT: the language highlighted by this mode is the CrabIR *textual
;; dump produced by Clam's `--ocrab' option*, i.e., the file created by
;;
;;     clam.py prog.c --crab-check=assert --ocrab=prog.crabir MORE_OPTIONS
;;
;; This is not a source language: it is an analysis report.  Besides the
;; CrabIR instructions themselves, an --ocrab file also contains the
;; annotations that Clam interleaves with the code:
;;
;;   * `/** INVARIANTS: ... **/' blocks, printed before/after each basic
;;     block, holding the abstract state inferred by the analysis (and,
;;     with --crab-print-voi, a `VARIABLES-OF-INFLUENCE:' section).
;;
;;   * `// loc(file=... line=... col=...) id=N Result:  OK' comments, printed
;;     just above every `assert', saying whether Clam discharged that
;;     assertion.  A failed check reads
;;     `Result:  FAIL -- num of warnings=1'.
;;
;; Those annotations are what you actually read an --ocrab file for, so this
;; mode gives them dedicated faces and navigation commands rather than
;; treating them as ordinary comments:
;;
;;   C-c C-n / C-c C-p   next/previous check result
;;   C-c C-f / C-c C-b   next/previous *failed* check result
;;   C-c C-i             hide/show the INVARIANTS blocks
;;
;; See also scripts/read_results.py, which summarizes the same `Result:'
;; comments from the command line.

;;; Code:

(require 'easymenu)
(require 'imenu)

(defgroup crab nil
  "Major mode for CrabIR dumps produced by Clam's --ocrab option."
  :group 'languages
  :prefix "crab-")

;;; Faces

(defface crab-label-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for CrabIR basic block labels."
  :group 'crab)

(defface crab-invariant-face
  '((t :inherit font-lock-doc-face))
  "Face for `/** INVARIANTS: ... **/' annotation blocks."
  :group 'crab)

(defface crab-check-ok-face
  '((((class color) (background light)) :foreground "#005f00" :weight bold)
    (((class color) (background dark))  :foreground "#5faf5f" :weight bold)
    (t :weight bold))
  "Face for a `Result:  OK' verification comment."
  :group 'crab)

(defface crab-check-fail-face
  '((((class color) (background light)) :foreground "#af0000" :weight bold)
    (((class color) (background dark))  :foreground "#ff5f5f" :weight bold)
    (t :inherit font-lock-warning-face))
  "Face for a `Result:  FAIL ...' verification comment."
  :group 'crab)

;;; Regexps for the --ocrab annotations

(defconst crab-check-regexp
  "^[ \t]*//.*\\bResult:"
  "Regexp matching a check-result comment emitted by --ocrab.")

(defconst crab-failed-check-regexp
  "^[ \t]*//.*\\bResult:[ \t]*FAIL"
  "Regexp matching a *failed* check-result comment emitted by --ocrab.")

(defconst crab-invariant-start-regexp
  "^[ \t]*/\\*\\* INVARIANTS:"
  "Regexp matching the start of an INVARIANTS annotation block.")

;;; Syntax table

(defvar crab-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C-style comments: /* ... */ (style a) and // ... eol (style b).
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; CrabIR identifiers are dotted and may carry `@' and `$', e.g. `p.addr.0',
    ;; `@V_4', `__@bb_5'.  Treating those characters as symbol constituents
    ;; makes `\_<' / `\_>' line up with real identifier boundaries, which keeps
    ;; keyword and number matching from firing inside a name (the `0' of `i.0'
    ;; is not an integer literal).
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?$ "_" table)
    table)
  "Syntax table for `crab-mode'.")

(defun crab--syntactic-face-function (state)
  "Return the face for the comment/string described by parser STATE.
Distinguishes Clam's `/** ... **/' annotation blocks from plain comments."
  (cond
   ((nth 3 state) font-lock-string-face)
   ((nth 4 state)
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at-p "/\\*\\*")
          'crab-invariant-face
        font-lock-comment-face)))))

;;; Font lock
;;
;; Instruction names below are kept in sync with crab's statement printers in
;; crab/include/crab/cfg/cfg.hpp.  Note that binary operators are printed as
;; symbols (`+ - * / /_u % %_u & | ^ << >>_l >>_r'), not as mnemonics, so there
;; is nothing word-like to highlight for arithmetic.

(defvar crab-font-lock-keywords
  `(;; Verification results: these live inside `//' comments, so they need
    ;; OVERRIDE to win over the syntactic comment face.
    ("\\(Result:\\)[ \t]*\\(OK\\)\\b"
     (1 'crab-check-ok-face t) (2 'crab-check-ok-face t))
    ("\\(Result:\\)[ \t]*\\(FAIL[^\n]*\\)"
     (1 'crab-check-fail-face t) (2 'crab-check-fail-face t))
    ;; Annotation section headers inside /** ... **/ blocks.
    ("\\_<\\(INVARIANTS\\|VARIABLES-OF-INFLUENCE\\|UNPROVEN ASSUMPTIONS\\):"
     (1 font-lock-keyword-face t))

    ;; Function declarations and call sites.
    ("\\_<\\(declare\\)[ \t]+\\([^( \t\n]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("\\_<\\(call\\)[ \t]+\\([^( \t\n]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ;; crab_intrinsic(NAME, ...)
    ("\\_<\\(crab_intrinsic\\)(\\([^,)\n]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-builtin-face))

    ;; Basic block labels.  Anchored to a whole line so that operand type
    ;; annotations such as `p.addr.0:ref' are not mistaken for labels.
    ("^\\([^ \t\n:]+\\):[ \t]*$" (1 'crab-label-face))
    ;; Branch targets.
    ("\\_<\\(goto\\)[ \t]+\\([^;\n]+\\)"
     (1 font-lock-keyword-face) (2 'crab-label-face))

    ;; Control and verification instructions.
    (,(regexp-opt '("assume" "unreachable" "havoc" "assert" "ite" "not") 'symbols)
     . font-lock-keyword-face)
    ;; Region and reference instructions.
    (,(regexp-opt '("region_init" "region_copy" "region_cast"
                    "make_ref" "remove_ref" "gep_ref"
                    "load_from_ref" "store_to_ref"
                    "ref_to_int" "int_to_ref")
                  'symbols)
     . font-lock-keyword-face)
    ;; Array instructions.
    (,(regexp-opt '("array_init" "array_store" "array_load" "array_assign")
                  'symbols)
     . font-lock-keyword-face)
    ;; Integer casts, printed as `trunc x:32 to y:8'.
    (,(regexp-opt '("trunc" "sext" "zext" "to") 'symbols)
     . font-lock-keyword-face)

    ;; Types.  The integer bitwidth is arbitrary (`int1', `int32', `int128'...),
    ;; so it is matched numerically rather than enumerated.
    ("\\_<int[0-9]*\\_>" . font-lock-type-face)
    (,(regexp-opt '("void" "bool" "real" "ref" "arr" "region" "unknown") 'symbols)
     . font-lock-type-face)

    ;; Constants.
    (,(regexp-opt '("NULL_REF" "true" "false") 'symbols) . font-lock-constant-face)
    ("[-+]oo\\_>" . font-lock-constant-face)
    ;; Allocation sites.
    ("\\_<as_[0-9]+\\_>" . font-lock-preprocessor-face)

    ;; Variables.
    ("@[-a-zA-Z$._][-a-zA-Z$._0-9]*" . font-lock-variable-name-face)
    ("\\_<\\.str\\.[0-9]+\\_>" . font-lock-variable-name-face)

    ;; Integer literals, last so that they do not shadow anything above.
    ("-?\\_<[0-9]+\\_>" . font-lock-constant-face))
  "Syntax highlighting for CrabIR as printed by Clam's --ocrab option.")

;;; Navigation over the --ocrab annotations

(defun crab--move-to-match (regexp count what)
  "Move COUNT matches of REGEXP, forward if positive.  WHAT names them."
  (let ((start (point))
        (search (if (> count 0) #'re-search-forward #'re-search-backward)))
    ;; Searching forward from inside a match would find the same one again.
    (when (> count 0)
      (end-of-line))
    (if (funcall search regexp nil t (abs count))
        (progn (beginning-of-line) (point))
      (goto-char start)
      (message "No more %s" what)
      nil)))

(defun crab-next-check (&optional n)
  "Move to the Nth next assertion check result printed by --ocrab."
  (interactive "p")
  (crab--move-to-match crab-check-regexp (or n 1) "check results"))

(defun crab-previous-check (&optional n)
  "Move to the Nth previous assertion check result printed by --ocrab."
  (interactive "p")
  (crab--move-to-match crab-check-regexp (- (or n 1)) "check results"))

(defun crab-next-failed-check (&optional n)
  "Move to the Nth next assertion that Clam could not discharge."
  (interactive "p")
  (crab--move-to-match crab-failed-check-regexp (or n 1) "failed checks"))

(defun crab-previous-failed-check (&optional n)
  "Move to the Nth previous assertion that Clam could not discharge."
  (interactive "p")
  (crab--move-to-match crab-failed-check-regexp (- (or n 1)) "failed checks"))

;;; Folding the INVARIANTS blocks
;;
;; Invariants dominate an --ocrab file by volume; being able to collapse them
;; and read the CrabIR alone is the main reason to open the file in Emacs.

(defvar-local crab--invariants-hidden nil
  "Non-nil when INVARIANTS annotation blocks are currently hidden.")

(defun crab--show-invariants ()
  "Reveal all INVARIANTS annotation blocks."
  (remove-overlays (point-min) (point-max) 'crab-invariant t))

(defun crab--hide-invariants ()
  "Hide all INVARIANTS annotation blocks."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward crab-invariant-start-regexp nil t)
      (let ((start (match-beginning 0))
            (end (if (re-search-forward "\\*\\*/" nil t)
                     (point)
                   (point-max))))
        ;; Swallow the trailing newline so the whole line disappears.
        (when (and (< end (point-max))
                   (eq (char-after end) ?\n))
          (setq end (1+ end)))
        (let ((ov (make-overlay start end)))
          (overlay-put ov 'crab-invariant t)
          (overlay-put ov 'invisible t)
          (overlay-put ov 'evaporate t))))))

(defun crab-toggle-invariants ()
  "Toggle visibility of the `/** INVARIANTS: ... **/' blocks."
  (interactive)
  (if crab--invariants-hidden
      (crab--show-invariants)
    (crab--hide-invariants))
  (setq crab--invariants-hidden (not crab--invariants-hidden))
  (message "CrabIR invariants %s"
           (if crab--invariants-hidden "hidden" "shown")))

;;; Keymap and menu

(defvar crab-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'crab-next-check)
    (define-key map (kbd "C-c C-p") #'crab-previous-check)
    (define-key map (kbd "C-c C-f") #'crab-next-failed-check)
    (define-key map (kbd "C-c C-b") #'crab-previous-failed-check)
    (define-key map (kbd "C-c C-i") #'crab-toggle-invariants)
    map)
  "Keymap for `crab-mode'.")

(easy-menu-define crab-mode-menu crab-mode-map
  "Menu for `crab-mode'."
  '("CrabIR"
    ["Next check result"      crab-next-check t]
    ["Previous check result"  crab-previous-check t]
    "---"
    ["Next failed check"      crab-next-failed-check t]
    ["Previous failed check"  crab-previous-failed-check t]
    "---"
    ["Toggle invariants"      crab-toggle-invariants t]))

;;; Mode

;;;###autoload
(define-derived-mode crab-mode prog-mode "CrabIR"
  "Major mode for CrabIR dumps produced by Clam's --ocrab option.

The buffer is expected to be a file created by

    clam.py prog.c --crab-check=assert --ocrab=prog.crabir MORE_OPTIONS

so it contains CrabIR instructions interleaved with the analysis
annotations Clam prints: `/** INVARIANTS: ... **/' blocks and
`// ... Result:  OK' / `// ... Result:  FAIL ...' comments above each
assertion.

\\{crab-mode-map}"
  :syntax-table crab-mode-syntax-table
  (setq-local font-lock-defaults
              '(crab-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . crab--syntactic-face-function)))
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s-*")
  (setq-local comment-end-skip "[ \t]*\\(?:\\*+/\\|\n\\)")
  ;; The dump is already formatted; there is nothing sensible to indent to.
  (setq-local indent-line-function #'ignore)
  (setq-local imenu-generic-expression
              '(("Function" "^.*\\_<declare\\_>[ \t]+\\([^( \t\n]+\\)" 1)
                ("Block"    "^\\([^ \t\n:]+\\):[ \t]*$" 1)))
  (setq-local outline-regexp "^[^ \t\n:]+:[ \t]*$"))

;;;###autoload
(defalias 'crabir-mode 'crab-mode)

;; Associate .crabir files (the conventional --ocrab output name) with crab-mode.
;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.crabir\\'") 'crab-mode))

(provide 'crab-mode)

;;; crab-mode.el ends here
