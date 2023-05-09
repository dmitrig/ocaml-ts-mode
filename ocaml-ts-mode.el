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

(defcustom ocaml-ts-mode-indent-offset 2
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

(defvar ocaml-ts-mode--dedent-regexp
  (concat "\\`"
          (regexp-opt
           '("and" "do" "done" "else" "end" "in" "then" "with"
             ")" "]" "|]" "}" "," "<-" "|"))
          "\\'")
  "Keywords to be indented evenly with parent.")

(defun ocaml-ts-mode--node-is (node type)
  (string-match-p
   type (or (treesit-node-type node) "")))

(defun ocaml-ts-mode--head-type (node)
  "Get type of node in head position of NODE."
  (let (next)
    (while (setq next (treesit-node-child node 0))
      (setq node next))
    (or (treesit-node-type node) "")))

(defun ocaml-ts-mode--default-offset (&optional offset)
  "Create OFFSET function for `treesit-simple-indent-rules'."
  (lambda (node &rest _)
    (or offset (setq offset 0))
    (cond
     ((ocaml-ts-mode--node-is node "infix_operator") offset)
     ((string-match-p ocaml-ts-mode--dedent-regexp (ocaml-ts-mode--head-type node)) offset)
     (t (+ ocaml-ts-mode-indent-offset offset)))))

(defconst ocaml-ts-mode--types-regexp
  (regexp-opt
   '("type_variable"
    "type_constructor_path"
    "constructed_type"
    ;; "polymorphic_variant_type"
    ;; "package_type"
    "hash_type"
    ;; "object_type"
    ;; "parenthesized_type"
    "tuple_type"
    "function_type"
    "aliased_type")
   'symbols)
  "All node types in _type.")

(defun ocaml-ts-mode--match-in-type (node &rest _)
  "Matcher to test if NODE is in a type expression."
  (string-match-p
   ocaml-ts-mode--types-regexp
   (or (treesit-node-type (treesit-node-parent node)) "")))

(defun ocaml-ts-mode--anchor-toplevel-type (node &rest _)
  "Get the top-most _type ancestor of NODE."
  (while-let ((parent (treesit-node-parent node))
              ((string-match-p
                ocaml-ts-mode--types-regexp
                (or (treesit-node-type parent) ""))))
    (setq node parent))
  (treesit-node-start node))

(defconst ocaml-ts-mode--aux-regexp
  (regexp-opt
   '("then_clause" "else_clause"  ; always in if_expression
     "do_clause"                  ; for_expression or while_expression
     ;; "binding" expressions: always in corresponding definition
     "let_binding"                ; value_definition (let _ = _)
     "module_binding"
     "class_binding" "class_type_binding"
     "type_binding"
     ;; align to enclosing type definition: not sure about other type decls
     "variant_declaration")
   'symbols)
  "Syntax nodes to be skipped when looking for parent nodes.")

(defun ocaml-ts-mode--parent-mod (node type)
  "Get parent of NODE modulo ignored node TYPE."
  (let ((parent (treesit-node-parent node)))
    (while (string-match-p type (or (treesit-node-type parent) ""))
      (setq parent (treesit-node-parent parent)))
    parent))

(defun ocaml-ts-mode--anchor-parent (node &rest _)
  (when-let ((parent (ocaml-ts-mode--parent-mod
                      node
                      ocaml-ts-mode--aux-regexp)))
    (treesit-node-start parent)))

(defconst ocaml-ts-mode--dangleable-regexp
  (regexp-opt
   '("structure"
     "signature"
     "class_body_type"
     "class_body_type"
     "object_expression"
     "fun_expression"
     "function_expression"
     "match_expression"
     "try_expression"
     "parenthesized_expression"
     "list_expression"
     "array_expression"
     "record_expression"
     "object_copy_expression"
     "record_declaration")
   'symbols))

(defconst ocaml-ts-mode--dangle-open-regexp
  (concat "\\`"
          (regexp-opt '("struct" "sig" "object"
                        "->" "function" "with"
                        "(" "[" "[|" "{" "{<"))
          "\\'")
  "Dangling expression \"openers\".")

(defun ocaml-ts-mode--dangling-p (node)
  "Check if a NODE is a \"dangling opener\"."
  (and (string-match-p ocaml-ts-mode--dangle-open-regexp
                       (treesit-node-type node))
       (save-excursion
         (goto-char (treesit-node-start (treesit-node-parent node)))
         ;; node not on its own line
         (not (looking-back (rx bol (* whitespace))
                            (line-beginning-position))))))

(defun ocaml-ts-mode--match-dangling-parent (_n parent &rest _)
  (string-match-p ocaml-ts-mode--dangleable-regexp
                  (treesit-node-type parent)))

(defun ocaml-ts-mode--parent-same-line (node type)
  (when-let* ((line (line-number-at-pos (treesit-node-start node)))
              (parent (ocaml-ts-mode--parent-mod node type))
              (pline (line-number-at-pos (treesit-node-start parent)))
             ((= pline line)))
    parent))

(defun ocaml-ts-mode--anchor-dangleable-parent (node &rest _)
  (catch 'term
    (let ((opener node))
      (while (setq opener (treesit-node-prev-sibling opener))
        (when (ocaml-ts-mode--dangling-p opener)
          ;; found the node of the anchor line
          (let ((parent (treesit-node-parent opener))
                (n 0))
            (while-let ((gp (ocaml-ts-mode--parent-same-line
                             parent ocaml-ts-mode--aux-regexp)))
              (setq parent gp)
              (setq n (1+ n)))
            (let ((start (+ (* n ocaml-ts-mode-indent-offset)
                            (treesit-node-start parent))))
              ;; TODO: remove; for debugging
              (pulse-momentary-highlight-region
               (treesit-node-start parent) start)
              (throw 'term start))))))
    (let ((parent (ocaml-ts-mode--parent-mod
                   node ocaml-ts-mode--aux-regexp)))
      (treesit-node-start parent))))

(defun ocaml-ts-mode--indent-rules (language)
  (let ((_log 'ocaml-ts-mode--log-anchor)
        (in-type 'ocaml-ts-mode--match-in-type)
        ;; (parent 'ocaml-ts-mode--anchor-parent)
        (dangle-parent 'ocaml-ts-mode--anchor-dangleable-parent)
        (top-type 'ocaml-ts-mode--anchor-toplevel-type)
        ;; (ofs ocaml-ts-mode-indent-offset)
        (default-ofs 'ocaml-ts-mode--default-offset))
    `((,language
       ((parent-is "compilation_unit") column-0 0)

       ;; indent children after "dangling opener"
       ;; (,dangling ,dangle-parent (,default-ofs))

       ;; align types evenly with top-level _type expr
       (,in-type ,top-type 0)

       ;; "special" expressions without indentation
       ((match nil "let_expression" nil 2) parent 0)
       ((match nil "let_\\(open\\|exception\\|module\\)_expression" nil 3) parent 0)
       ((parent-is "sequence_expression") parent 0)

       (catch-all ,dangle-parent (,default-ofs))))))

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

   ;; :language language
   ;; :feature 'type
   ;; :override t
   ;; '((_expression) @custom-invalid)

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
  (regexp-opt '("type_binding"
                "exception_definition"
                "external"
                "let_binding"
                "value_specification"
                "method_definition"
                "method_specification"
                "instance_variable_definition"
                "instance_variable_specification"
                "module_binding"
                "module_type_definition"
                "class_binding"
                "class_type_binding"))
  "Regex used to find defun-like nodes.")

(defun ocaml-ts-mode--defun-valid-p (node)
  "Predicate to check if NODE is really defun-like."
  (and (treesit-node-check node 'named)
       (not (treesit-node-top-level
             node (regexp-opt '("let_expression"
                                "parenthesized_module_expression"
                                "package_expression")
                              'symbols)))))

(defun ocaml-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "type_binding"
         "method_definition"
         "instance_variable_definition"
         "module_binding"
         "module_type_definition"
         "class_binding"
         "class_type_binding")
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("exception_definition"
     (treesit-node-text
      (treesit-search-subtree node "constructor_name" nil nil 2) t))
    ("external"
     (treesit-node-text
      (treesit-search-subtree node "value_name" nil nil 1) t))
    ("let_binding"
     (treesit-node-text
      (treesit-node-child-by-field-name node "pattern") t))
    ("value_specification"
     (treesit-node-text
      (treesit-search-subtree node "value_name" nil nil 1) t))
    ("method_specification"
     (treesit-node-text
      (treesit-search-subtree node "method_name" nil nil 1) t))
    ("instance_variable_specification"
     (treesit-node-text
      (treesit-search-subtree node "instance_variable_name" nil nil 1) t))))

(defun ocaml-ts-mode--imenu-name (node)
  "Return qualified defun name of NODE."
  (let ((name nil))
    (while node
      (when-let ((new-name (treesit-defun-name node)))
        (if name
            (setq name (concat new-name
                               treesit-add-log-defun-delimiter
                               name))
          (setq name new-name)))
      (setq node (treesit-node-parent node)))
    name))

;; TODO: could add constructors / fields
(defvar ocaml-ts-mode--imenu-settings
  `(("Type" "\\`type_binding\\'"
     ocaml-ts-mode--defun-valid-p ocaml-ts-mode--imenu-name)
    ("Spec" "\\`\\(value_specification\\|method_specification\\)\\'"
     ocaml-ts-mode--defun-valid-p ocaml-ts-mode--imenu-name)
    ("Exception" "\\`exception_definition\\'"
     ocaml-ts-mode--defun-valid-p ocaml-ts-mode--imenu-name)
    ("Value" "\\`\\(let_binding\\|external\\)\\'"
     ocaml-ts-mode--defun-valid-p ocaml-ts-mode--imenu-name)
    ("Method" "\\`\\(method_definition\\)\\'"
     ocaml-ts-mode--defun-valid-p ocaml-ts-mode--imenu-name)
    ;; grouping module/class types under Type causes some weird nesting
    ("Module" "\\`\\(module_binding\\|module_type_definition\\)\\'"
     ocaml-ts-mode--defun-valid-p nil)
    ("Class" "\\`\\(class_binding\\|class_type_binding\\)\\'"
     ocaml-ts-mode--defun-valid-p ocaml-ts-mode--imenu-name))
  "Settings for `treesit-simple-imenu'.")

(defun ocaml-ts-mode--comment-setup ()
  "Common locals for comments."
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*"))

(defvar ocaml-ts-mode--block-regex
  (regexp-opt `(,@ocaml-ts-mode--keywords
                "do_clause"
                ;; "if_expression"
                ;; "fun_expression"
                ;; "match_expression"
                "local_open_expression"
                "coercion_expression"
                "array_expression"
                "list_expression"
                "parenthesized_expression"
                "parenthesized_pattern"
                "match_case"
                "parameter"
                ;; "value_definition"
                "let_binding"
                "value_specification"
                "value_name"
                "label_name"
                "constructor_name"
                "module_name"
                "module_type_name"
                "value_pattern"
                "value_path"
                "constructor_path"
                "infix_operator"
                "number" "boolean" "unit"
                "type_definition"
                "type_constructor"
                ;; "module_definition"
                "package_expression"
                "typed_module_expression"
                "module_path"
                "signature"
                "structure"
                "string" "quoted_string" "character")
              'symbols))

(defun ocaml-ts-mode-forward-sexp (arg)
  "Implement `forward-sexp-function'.ARG is passed to `treesit-end-of-thing'."
  (if (< arg 0)
      (treesit-beginning-of-thing ocaml-ts-mode--block-regex (- arg))
    (treesit-end-of-thing ocaml-ts-mode--block-regex arg)))

;;;###autoload
(define-derived-mode ocaml-ts-mode prog-mode "OCaml"
  "Major mode for editing OCaml, powered by tree-sitter."
  :group 'ocaml
  :syntax-table ocaml-ts-mode--syntax-table

  (unless (treesit-ready-p 'ocaml)
    (error "Tree-sitter for OCaml isn't available"))

  (treesit-parser-create 'ocaml)

  ;; Comments.
  (ocaml-ts-mode--comment-setup)

  ;; Indent.
  (setq-local treesit-simple-indent-rules
              (ocaml-ts-mode--indent-rules 'ocaml))

  ;; Navigation.
  (setq-local forward-sexp-function #'ocaml-ts-mode-forward-sexp)
  (setq-local treesit-defun-type-regexp
              (cons ocaml-ts-mode--defun-type-regexp
                    #'ocaml-ts-mode--defun-valid-p))
  (setq-local treesit-defun-name-function #'ocaml-ts-mode--defun-name)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings
              (ocaml-ts-mode--font-lock-settings 'ocaml))
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string)
                (attribute builtin constant type)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings ocaml-ts-mode--imenu-settings)

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
  (ocaml-ts-mode--comment-setup)

  ;; Indent.
  (setq-local treesit-simple-indent-rules
              (ocaml-ts-mode--indent-rules 'ocaml-interface))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (cons ocaml-ts-mode--defun-type-regexp
                    #'ocaml-ts-mode--defun-valid-p))
  (setq-local treesit-defun-name-function #'ocaml-ts-mode--defun-name)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings
              (ocaml-ts-mode--font-lock-settings 'ocaml-interface))
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string)
                (attribute builtin constant type)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings ocaml-ts-mode--imenu-settings)

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
