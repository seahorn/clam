;;; crab-mode.el --- Major mode for the CrabIR language.

(defvar crab-font-lock-keywords
  (list
   ;; Comments
   '("\/\\*[^*]*\\*\/" . font-lock-comment-face)
   '("\/\\*\\*[^*]*\\*\\*\/" . font-lock-comment-face)
   ;; Variables
   '("@[-a-zA-Z$\._][-a-zA-Z$\._0-9]*" . font-lock-variable-name-face)
   '(".str.[0-9]+" . font-lock-variable-name-face)   
   ;; Labels
   '("[-a-zA-Z$\._0-9]+:" . font-lock-variable-name-face)
   ;; Types
   `(,(regexp-opt '("void" "bool" "int" "int8" "int16" "int32" "int64" "int128" "ref" "region" "unknown") 'symbols) . font-lock-type-face)
   ;; Allocation site constants
   '("\\bas_[0-9]+\\b" . font-lock-preprocessor-face)      
   ;; Integer literals
   '("\\b[-]?[0-9]+\\b" . font-lock-preprocessor-face)
   ;; Keywords
   `(,(regexp-opt '("declare" "NULL_REF" "true" "false") 'symbols) . font-lock-keyword-face)
   ;; Arithmetic and Logical Operators
   ;;`(,(regexp-opt '("add" "sub" "mul" "sdiv" "udiv" "urem" "srem" "and" "or" "xor") 'symbols) . font-lock-keyword-face)
   ;; Special instructions
   `(,(regexp-opt '("call" "ite") 'symbols) . font-lock-keyword-face)
   ;; Control instructions
   `(,(regexp-opt '("assume" "goto" "unreachable") 'symbols) . font-lock-keyword-face)   
   ;; Verification instructions
   `(,(regexp-opt '("havoc" "assert" "crab_intrinsic") 'symbols) . font-lock-keyword-face)
   ;; Array operations
   `(,(regexp-opt '("array_store" "array_load" "array_assign") 'symbols) . font-lock-keyword-face)
   ;; Boolean operations
   `(,(regexp-opt '("not") 'symbols) . font-lock-keyword-face)
   ;; Memory operators
   `(,(regexp-opt '("region_init" "region_copy" "region_cast" "make_ref" "remove_ref" "load_from_ref" "store_to_ref" "gep_ref" "load_from_arr_ref" "store_to_arr_ref" "ref_to_int" "int_to_ref") 'symbols) . font-lock-keyword-face)
   ;; Casts
   `(,(regexp-opt '("trunc" "sext" "zext") 'symbols) . font-lock-keyword-face))
  "Syntax highlighting for CrabIR.")

;; Emacs 23 compatibility.
(defalias 'crab-mode-prog-mode
  (if (fboundp 'prog-mode)
      'prog-mode
    'fundamental-mode))

;;;###autoload
(define-derived-mode crab-mode crab-mode-prog-mode "Crab"
  "Major mode for editing Crab source files."
  
  (setq font-lock-defaults `(crab-font-lock-keywords))
  ;;; it doesn't work
  ;;; (setq comment-start "\\(//+\\|/\\*+\\)\\s *" comment-end "*/")
  )

;;; (set-face-foreground 'font-lock-comment-face "red3")

;; Associate .crabir files with crab-mode
;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.crabir\\'")  'crab-mode))

(provide 'crab-mode)

;;; crab-mode.el ends here
