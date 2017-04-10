(require 'dash)

(defalias fbp-custom-var #'customize-set-variable)

(defun fbp-custom-vars (&rest ASSOCS)
  "For each (SYMBOL . VALUE) of ASSOCS, customize SYMBOL to VALUE with `customize-set-variable'.
Example:
 (fbp-custom-vars
  '(foo . 1)
  '(bar . (A B C)) ; hopefully more clear than '(bar A B C)
 )"
  (dolist (pair ASSOCS)
    (fbp-custom-var (car pair) (cdr pair))))

(defun fbp-def-face (GROUP SYMBOL DOCSTRING &rest PROPERTIES)
  "Create a face called SYMBOL with DOCSTRING and PROPERTIES in customization group GROUP.
The display is always t. If you are worried about unsupported properties, use `display-supports-face-attributes-p' to filter PROPERTIES.
Example:
 (fbp-def-face 'my-lame-stuff
               'my-lame-face
               \"a lame face\"
               :weight 'ultra-light
               :box t)"
  (custom-declare-face SYMBOL
                       (list (cons t PROPERTIES))
                       DOCSTRING
                       :group GROUP))

(defun fbp-def-faces (GROUP &rest FACES)
  "For each face of the form (SYMBOL DOCSTRING . PROPERTIES) in FACES, use `fbp-def-face' to create a face called SYMBOL with DOCSTRING and PROPERTIES in customization group GROUP.
Example:
 (fbp-def-faces 'my-lame-stuff
    '(my-lame-face \"a lame face\" :weight ultra-light :box t)
    '(my-reverse-face \"a reversed face\" :slant reverse-oblique :inverse-video t :inherit my-lame-face)
 )"
  (declare (indent 2))
  (dolist (face FACES)
    (apply #'fbp-def-face GROUP face)))
