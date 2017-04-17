;;; fbp.el -- Cut the cruft.

(mapcar #'require
        '(cl-lib dash))

(defmacro fbp-let (BINDINGS &rest BODY)
  "Locally bind variables and functions.
Variable bindings take the form (SYMBOL VALUE).
Function bindings take the form (SYMBOL ARGS BODY)."
  (declare (indent 1))
  (let ((funcs nil)
        (vars nil))
    (dolist (binding BINDINGS)
      (cl-case (length binding)
        (3 (push binding funcs))
        (2 (push binding vars))
        (otherwise (error "Invalid binding %s" binding))))
    `(cl-flet ,funcs
       (let ,vars
         ,@BODY))))

(defun fbp-coerce-name (X)
  "Turn whatever into a string that looks OK."
  (pcase X
    ((pred stringp) X)
    ((pred keywordp) (substring (symbol-name X) 1))
    (_ (prin1-to-string X t))))

(defun nonempty (X)
  "Return nil if X is a common empty container, otherwise return X."
  (unless (memq X '("" [])) X))

(defun fbp-intercalate (ELT LIST &rest FLAGS)
  "Insert ELT between each element of LIST.
First removes all empty elements from LIST, unless passed the :keep-empty flag."
  (let ((l (if (memq :keep-empty FLAGS)
               LIST
               (-filter #'nonempty LIST))))
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

(defun fbp-custom-vars (&rest ASSOCS)
  "For each (SYMBOL . VALUE) of ASSOCS, customize SYMBOL to VALUE with `customize-set-variable'.
Example:
 (fbp-custom-vars
  '(foo . 1)
  '(bar . (A B C)) ; hopefully more clear than '(bar A B C)
 )"
  (dolist (pair ASSOCS)
    (customize-set-variable (car pair) (cdr pair))))

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
  (let* (
         (prefix (fbp-concat-symbol-name GROUP NAME))
         (active-face (intern (concat prefix "-active-face")))
         (inactive-face (intern (concat prefix "-inactive-face")))
         (proc-sym (intern (concat prefix "-face")))
         )
    (fbp-def-faces GROUP
      (cons active-face ACTIVE-DOC.PROPS)
      (cons inactive-face INACTIVE-DOC.PROPS))
    (fset proc-sym
          `(lambda ()
             (if (funcall ,TEST)
                 ,active-face
                 ,inactive-face)))
    proc-sym))

(defun fbp-make-hook (WHEN PROCEDURE &optional CONTINGENT)
  "Create hook WHEN-PROCEDURE-hook to run WHEN PROCEDURE is called, unless it is already defined. The CONTINGENT functions are added to the hook regardless."
  (let* ((when-str (substring (symbol-name WHEN) 1))
         (proc-name (symbol-name PROCEDURE))
         (hook-name (concat when-str "-" proc-name "-hook"))
         (existing-hook (intern-soft hook-name))
         (hook-symbol (or existing-hook (intern hook-name))))
    (unless existing-hook
      (set hook-symbol nil)
      (put hook-symbol 'variable-documentation
           (concat "procedures to run " when-str " `" proc-name "'"))
      (advice-add
       PROCEDURE
       WHEN
       (lambda (&rest _)
         (run-hooks `,hook-symbol))))
    (dolist (contingent-proc (reverse CONTINGENT))
      (add-hook hook-symbol contingent-proc))
    hook-symbol))

(provide 'fbp)
