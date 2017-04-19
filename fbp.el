;;; fbp.el -- Cut the cruft.

(eval-when-compile
  (require 'cl-lib))

(mapcar #'require
        '(dash dash-functional))

(or (fboundp '-fn)
    (defalias '-fn '-lambda))

;; (defun fbp--indent-plist (PATH STATE INDENT-POINT SEXP-COLUMN NORMAL-INDENT)
;;   "Properly indent pairs of things in lists.
;; PATH is a list of integers that represents the list structure enclosing INDENT-POINT. For example, in ((a b c (d X) f) g), X has a path of (0 3 1) -- paths are zero-indexed.
;; STATE is the `parse-partial-sexp' state at INDENT-POINT.
;; INDENT-POINT is the point where the function is called.
;; SEXP-COLUMN is the column of the opening parenthesis of the innermost containing list.
;; NORMAL-INDENT is 'the column the indentation point was originally in'."
;;   )


(defun fbp--pairs (LIST &optional REMAINDER-FUNC)
  "Pairs up the elements of LIST in a new list. If there are an odd numbers of elements and REMAINDER-FUNC is included, it is applied to the last element and the result is the last element of the new list. Otherwise the extra element is ignored."
  (when LIST
    (if (cdr LIST)
        (cons (list (car LIST) (cadr LIST))
              (fbp--pairs (cddr LIST) REMAINDER-FUNC))
      (when REMAINDER-FUNC
        (cons (funcall REMAINDER-FUNC (car LIST)) nil)))))

(defmacro fbp-if (&rest CONDITIONS)
  "Use:
  (fbp-if
    CONDITION_1 RESULT_1
    CONDITION_2 RESULT_2
    ...
    RESULT_N)
Return the first RESULT for which CONDITION is true.
If CONDITIONS has an even number of elements, RESULT_N is nil."
  (declare (indent 0))
  `(cond ,@(fbp--pairs CONDITIONS (-fn (r) (list t r)))))

(defmacro fbp-let (&rest BINDINGS.EXPR)
  "`-let' with fewer parentheses.
Use:
  (fbp-let
    VAR_1 VAL_1
    VAR_2 VAL_2
    ...
    VAR_N VAL_N
    EXPR)
Evaluate EXPR with VARs bound to VALs."
  (declare (indent 0))
  (let ((bindings (fbp--pairs BINDINGS.EXPR))
        (expr (last BINDINGS.EXPR)))
    `(-let ,bindings ,@expr)))


(defmacro fbp-let* (&rest BINDINGS.EXPR)
  "`-let*' with fewer parentheses.
Use:
  (fbp-let
    VAR_1 VAL_1
    VAR_2 VAL_2
    ...
    VAR_N VAL_N
    EXPR)
Bind each VAR to its VAL in sequence, then evalueate EXPR."
  (declare (indent 0))
  (fbp-let bindings (fbp--pairs BINDINGS.EXPR)
           expr (last BINDINGS.EXPR)
           `(-let* ,bindings ,@expr)))

(defmacro fbp-let-both (BINDINGS &rest BODY)
  "Locally bind variables and functions.
Variable bindings take the form (SYMBOL VALUE).
Function bindings take the form (SYMBOL ARGS BODY)."
  (declare (indent 0))
  (fbp-let
    funcs nil
    vars nil
    (progn
      (dolist (binding BINDINGS)
        (cl-case (length binding)
          (3 (push binding funcs))
          (2 (push binding vars))
          (otherwise (error "Invalid binding %s" binding))))
      `(cl-flet ,funcs
         (-let ,vars
           ,@BODY)))))

(defmacro fbp-do-with (EXPR &rest BINDINGS)
  (declare (indent (&whole 4 &rest -2)))
  (fbp-let
    (vars funcs) (--separate (= 2 (length it)) BINDINGS)
    `(cl-flet ,funcs
       (-let ,vars
         ,EXPR))))

(defmacro fbp-case (VAL &rest CONDITIONS)
  (declare (indent 1))
  `(pcase ,VAL
     ,@(fbp--pairs CONDITIONS (-fn (X) `(_ ,X)))))

(defun fbp-coerce-name (X)
  "Turn whatever into a string that looks OK."
  (fbp-case X
    (pred stringp) X
    (pred keywordp) (substring (symbol-name X) 1)
    (prin1-to-string X t)))

(defun nonempty (X)
  "Return nil if X is a common empty container, otherwise return X."
  (unless (memq X '("" [])) X))

(defun fbp-intercalate (ELT LIST &rest FLAGS)
  "Insert ELT between each element of LIST.
First removes all empty elements from LIST, unless passed the :keep-empty flag."
  (fbp-let
   l (if (memq :keep-empty FLAGS)
         LIST
       (-filter #'nonempty LIST))
   (--reduce-r-from (cons it (when acc (cons ELT acc)))
                    nil
                    l)))

(defun fbp-concat-name (&rest ARGS)
  "Concatenate ARGS into a string."
  (apply #'concat
         (mapcar #'fbp-coerce-name ARGS)))

(defun fbp-concat-symbol-name (&rest ARGS)
  "Concatenate stuff into something suitable to pass to `make-symbol' or `intern'."
  (apply #'fbp-concat-name (fbp-intercalate "-" ARGS)))

(defun fbp-concat-symbol (&rest ARGS)
  "Concatenate ARGS into an uninterned symbol."
  (make-symbol (apply #'fbp-concat-symbol-name ARGS)))

(defun fbp-concat-intern (&rest ARGS)
  "Concatenate ARGS into an interned symbol."
  (intern (apply #'fbp-concat-symbol-name ARGS)))

(defun fbp-custom-vars (&rest BINDINGS)
  "For each SYMBOL VALUE of BINDINGS, customize SYMBOL to VALUE with `customize-set-variable'.
Example:
 (fbp-custom-vars
  'foo 1
  'bar '(A B C)
 )"
  (fbp-let
   pairs (fbp--pairs BINDINGS)
   (dolist (pair pairs)
     (customize-set-variable (car pair) (cdr pair)))))

(defun fbp-def-face (GROUP SYMBOL DOCSTRING &rest PROPERTIES)
  "Create a face called SYMBOL with documentation DOCSTRING and properties PROPERTIES in customization group GROUP.
Example:
  (fbp-def-face 'my-lame-stuff 'my-lame-face \"a lame face\" :weight 'ultra-light :box t)"
  (custom-declare-face SYMBOL
                       (list (cons t PROPERTIES))
                       DOCSTRING
                       :group GROUP))

(defun fbp-def-faces (GROUP &rest FACES)
  "For each face of the form (SYMBOL DOCSTRING . PROPERTIES) in FACES, create a face called SYMBOL with DOCSTRING and PROPERTIES in customization group GROUP.
Example:
 (fbp-def-faces 'my-lame-stuff
    '(my-lame-face \"a lame face\" :weight ultra-light :box t)
    '(my-reverse-face \"a reversed face\" :slant reverse-oblique :inverse-video t :inherit my-lame-face)
    )"
  (declare (indent 1))
  (--map (apply #'fbp-def-face GROUP it)
         FACES))

(defun fbp-make-face-adaptive (GROUP TEST ACTIVE-DOC.PROPS INACTIVE-DOC.PROPS &optional NAME)
  "Create GROUP-NAME-active-face and GROUP-NAME-inactive-face, and a function that returns one of them based on TEST."
  (fbp-let*
    prefix (fbp-concat-symbol-name GROUP NAME)
    active-face (intern (concat prefix "-active-face"))
    inactive-face (intern (concat prefix "-inactive-face"))
    proc-sym (intern (concat prefix "-face"))
    (progn
      (fbp-def-faces GROUP
        (cons active-face ACTIVE-DOC.PROPS)
        (cons inactive-face INACTIVE-DOC.PROPS))
      (fset proc-sym
            `(-fn ()
                  (if (funcall ,TEST)
                      ,active-face
                    ,inactive-face)))
      proc-sym)))

(defun fbp-make-hook (WHEN PROCEDURE &optional CONTINGENT)
  "Create hook WHEN-PROCEDURE-hook to run WHEN PROCEDURE is called, unless it is already defined. The CONTINGENT functions are added to the hook regardless."
  (fbp-let*
    when-str (substring (symbol-name WHEN) 1)
    proc-name (symbol-name PROCEDURE)
    hook-name (concat when-str "-" proc-name "-hook")
    existing-hook (intern-soft hook-name)
    hook-symbol (or existing-hook (intern hook-name))
    (progn
      (unless existing-hook
        (set hook-symbol nil)
        (put hook-symbol 'variable-documentation
             (concat "procedures to run " when-str " `" proc-name "'"))
        (advice-add
         PROCEDURE
         WHEN
         (-fn (&rest _)
              (run-hooks `,hook-symbol))))
      (dolist (contingent-proc (reverse CONTINGENT))
        (add-hook hook-symbol contingent-proc))
      hook-symbol)))

(provide 'fbp)
