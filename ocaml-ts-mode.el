;;; ocaml-ts-mode.el --- tree-sitter support for OCaml  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Author: dmitrig
;; URL: https://github.com/dmitrig/ocaml-ts-mode
;; Keywords: ocaml languages tree-sitter
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; WIP

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-query-compile "treesit.c")

(defcustom ocaml-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `ocaml-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'ocaml)

(defvar ocaml-ts-mode--syntax-table     ; adapted from tuareg.el
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?' "_" st)     ; ' is part of symbols
    (modify-syntax-entry ?. "'" st)     ; qualified ids are a single symbol
    (dolist (c '(?# ?` ?! ?$ ?% ?& ?+ ?- ?/ ?: ?< ?= ?> ?@ ?^ ?|))
      (modify-syntax-entry c "." st))
    (modify-syntax-entry ?? ". p" st)   ; label prefix chars
    (modify-syntax-entry ?~ ". p" st)
    (modify-syntax-entry ?\" "\"" st)   ; string delimiter
    (modify-syntax-entry ?\\ "\\" st)   ; escape char
    (modify-syntax-entry ?*  ". 23" st) ; comment delimiters
    (modify-syntax-entry ?\( "()1n" st)
    (modify-syntax-entry ?\) ")(4n" st)
    st)
  "Syntax table for `ocaml-ts-mode'.")

(defvar ocaml-ts-mode--indent-rules
  `((ocaml))
  "Tree-sitter indent rules.")

(defvar ocaml-ts-mode--keywords
  '("and" "as" "assert" "begin" "class" "constraint" "do" "done"
    "downto" "else" "end" "exception" "external" "for" "fun"
    "function" "functor" "if" "in" "include" "inherit" "initializer"
    "lazy" "let" "match" "method" "module" "mutable" "new" "nonrec"
    "object" "of" "open" "private" "rec" "sig" "struct" "then" "to"
    "try" "type" "val" "virtual" "when" "while" "with")
  "OCaml keywords for tree-sitter font-locking.")

(defvar ocaml-ts-mode--constants
  '((unit) "true" "false")
  "OCaml constants for tree-sitter font-locking.")

;; already all parsed as infix_operator?
;; (defvar ocaml-ts-mode--operators
;;   '("asr" "land" "lor" "lsl" "lsr" "lxor" "or" "mod")
;;   "OCaml operators for tree-sitter font-locking.")

(defvar ocaml-ts-mode--builtin-ids
  '("raise" "raise_notrace" "invalid_arg" "failwith" "ignore" "ref"
    "exit" "at_exit"
    ;; builtin exceptions
    "Exit" "Match_failure" "Assert_failure" "Invalid_argument"
    "Failure" "Not_found" "Out_of_memory" "Stack_overflow" "Sys_error"
    "End_of_file" "Division_by_zero" "Sys_blocked_io"
    "Undefined_recursive_module"
    ;; parser access
    "__LOC__" "__FILE__" "__LINE__" "__MODULE__" "__POS__"
    "__FUNCTION__" "__LOC_OF__" "__LINE_OF__" "__POS_OF__")
  "OCaml builtin identifiers for tree-sitter font-locking.")

(defun ocaml-ts-mode--font-lock-settings (language)
  "Tree-sitter font-lock settings for LANGUAGE."
  (treesit-font-lock-rules
   :language language
   :feature 'comment
   '((((comment) @font-lock-doc-face)
      (:match "^(\\*\\*[^*]" @font-lock-doc-face))
     (comment) @font-lock-comment-face)

   :language language
   :feature 'definition
   '(;; let-bound functions and variables, methods
     (let_binding pattern: (value_name) @font-lock-variable-name-face (":" (_)) :? (":>" (_)) :? :anchor body: (_))
     (let_binding pattern: (value_name) @font-lock-function-name-face (parameter)+)
     (method_definition (method_name) @font-lock-function-name-face)
     (method_specification (method_name) @font-lock-function-name-face)
     ;; patterns containing bound variables
     (value_pattern) @font-lock-variable-name-face
     (constructor_pattern pattern: (value_name) @font-lock-variable-name-face)
     (tuple_pattern (value_name) @font-lock-variable-name-face)
     ;; punned record fields in patterns
     (field_pattern (field_path (field_name) @font-lock-variable-name-face) :anchor)
     (field_pattern (field_path (field_name) @font-lock-variable-name-face) (type_constructor_path) :anchor)
     ;; signatures and misc
     (instance_variable_name) @font-lock-variable-name-face
     (value_specification (value_name) @font-lock-variable-name-face)
     (external (value_name) @font-lock-variable-name-face))

   :language language
   :feature 'keyword
   `([,@ocaml-ts-mode--keywords] @font-lock-keyword-face
     (fun_expression "->" @font-lock-keyword-face)
     (match_case "->" @font-lock-keyword-face))

   :language language
   :feature 'attribute
   '((attribute) @font-lock-preprocessor-face
     (item_attribute) @font-lock-preprocessor-face
     (floating_attribute) @font-lock-preprocessor-face)

   :language language
   :feature 'string
   :override t
   '([(string) (quoted_string) (character)] @font-lock-string-face)

   :language language
   :feature 'builtin
   `([";;"] @font-lock-preprocessor-face
     ((value_path :anchor (value_name) @font-lock-builtin-face)
      (:match ,(regexp-opt ocaml-ts-mode--builtin-ids 'symbols) @font-lock-builtin-face))
     ((constructor_path :anchor (constructor_name) @font-lock-builtin-face)
      (:match ,(regexp-opt ocaml-ts-mode--builtin-ids 'symbols) @font-lock-builtin-face)))

   :language language
   :feature 'constant
   `(;; some literals TODO: any more?
     [,@ocaml-ts-mode--constants] @font-lock-constant-face
     ;; doesn't look great
     ;; (constructor_name) @font-lock-constant-face
     ;; (method_invocation (method_name) @font-lock-constant-face)
     ;; TODO: highlight just alpha infix at lvl 3 to match tuareg?
     (method_invocation "#" @font-lock-operator-face)
     (infix_operator) @font-lock-operator-face
     (prefix_operator) @font-lock-operator-face)

   :language language
   :feature 'type
   '([(type_constructor) (type_variable) (hash_type)
      (class_name) (class_type_name)] @font-lock-type-face
     (function_type "->" @font-lock-type-face)
     (tuple_type "*" @font-lock-type-face)
     (polymorphic_variant_type ["[>" "[<" ">" "|" "[" "]"] @font-lock-type-face)
     (object_type ["<" ">" ";" ".."] @font-lock-type-face)
     (constructor_declaration ["->" "*"] @font-lock-type-face)
     (record_declaration ["{" "}" ";"] @font-lock-type-face)
     (parenthesized_type ["(" ")"] @font-lock-type-face)
     (polymorphic_type "." @font-lock-type-face)
     (module_name) @font-lock-type-face
     (module_type_name) @font-lock-type-face)))

(defvar ocaml-ts-mode--defun-type-regexp
  (regexp-opt '("type_definition"
                "value_definition"
                "value_specification"
                "module_definition"
                "module_type_definition"
                "class_definition"
                "class_type_definition"
                "method_definition"
                "method_specification"
                "instance_variable_definition"
                "instance_variable_specification"
                "external"))
  "Regex used to fund defun-like nodes.")

(defun ocaml-ts-mode--defun-pred (node)
  "Predicate to check if NODE is really defun-like."
  (not (string-equal (treesit-node-type (treesit-node-parent node))
                     "let_expression")))

;;;###autoload
(define-derived-mode ocaml-ts-mode prog-mode "OCaml"
  "Major mode for editing OCaml, powered by tree-sitter."
  :group 'ocaml
  :syntax-table ocaml-ts-mode--syntax-table

  (unless (treesit-ready-p 'ocaml)
    (error "Tree-sitter for OCaml isn't available"))

  (treesit-parser-create 'ocaml)

  ;; Comments.
  ;; TODO

  ;; Indent.
  (setq-local treesit-simple-indent-rules ocaml-ts-mode--indent-rules)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (cons ocaml-ts-mode--defun-type-regexp
                    #'ocaml-ts-mode--defun-pred))
  (setq-local treesit-defun-name-function nil)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings
              (ocaml-ts-mode--font-lock-settings 'ocaml))
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string)
                (attribute builtin constant type)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings nil)

  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode ocamli-ts-mode prog-mode "OCaml[mli]"
  "Major mode for editing OCaml, powered by tree-sitter."
  :group 'ocaml
  :syntax-table ocaml-ts-mode--syntax-table

  (unless (treesit-ready-p 'ocaml-interface)
    (error "Tree-sitter for OCaml isn't available"))

  (treesit-parser-create 'ocaml-interface)

  ;; Comments.
  ;; TODO

  ;; Indent.
  (setq-local treesit-simple-indent-rules nil)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (cons ocaml-ts-mode--defun-type-regexp
                    #'ocaml-ts-mode--defun-pred))
  (setq-local treesit-defun-name-function nil)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings
              (ocaml-ts-mode--font-lock-settings 'ocaml-interface))
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string)
                (attribute builtin constant type)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings nil)

  (treesit-major-mode-setup))

(defvar ocaml-ts-mode--grammar-repo
  "https://github.com/tree-sitter/tree-sitter-ocaml")

(defvar ocaml-ts-mode--source-alist
  `((ocaml . (,ocaml-ts-mode--grammar-repo nil "ocaml/src"))
    (ocaml-interface . (,ocaml-ts-mode--grammar-repo nil "interface/src"))))

;;;###autoload
(defun ocaml-ts-mode-install-grammar ()
  "Install tree-sitter-ocaml grammars."
  (interactive)
  (when (y-or-n-p (format "Install grammar from %s?"
                          ocaml-ts-mode--grammar-repo))
    (let ((treesit-language-source-alist ocaml-ts-mode--source-alist))
      (treesit-install-language-grammar 'ocaml)
      (treesit-install-language-grammar 'ocaml-interface))))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.ml\\'" . ocaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . ocamli-ts-mode)))

(provide 'ocaml-ts-mode)

;;; ocaml-ts-mode.el ends here
