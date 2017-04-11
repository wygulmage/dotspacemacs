;;; fbp.el -- Cut the cruft.

(require 'dash)

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
  (declare (indent 2))
  (mapcar (lambda (face)
            (apply #'fbp-def-face GROUP face))
          FACES))

(defun fbp-make-face-adaptive (GROUP TEST ACTIVE-DOC.PROPS INACTIVE-DOC.PROPS &optional NAME)
  "Create GROUP-NAME-active-face and GROUP-NAME-inactive-face, and a function that returns one of them based on TEST."
  (-let* (
          (prefix (concat (symbol-name GROUP)
                          "-"
                          (when NAME (concat NAME "-"))))
          ((active-face inactive-face)
           (fbp-def-faces
            GROUP
            (cons (intern (concat prefix "active-face"))
                  ACTIVE-DOC.PROPS)
            (cons (intern (concat prefix "inactive-face"))
                  INACTIVE-DOC.PROPS)))
          (proc-sym (intern (concat prefix "face")))
          )
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
