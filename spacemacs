;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup. It must be stored in your home directory.
(defun dotspacemacs/layers ()
  "Configuration layers:
This function should only set values."
  (setq-default
   ;; Base setup, a layer contained in the directory `+distribution':
   dotspacemacs-distribution
   ;; 'spacemacs ; (default)
   'spacemacs-base ; minimal

   ;; Layer installation & uninstalling:
   dotspacemacs-install-packages
   ;; 'used-only ; (default) installs only explicitly used packages and uninstalls others.
   'used-but-keep-unused ; installs only explicitly used packages but keeps others.
   ;; 'all ; installs all supported packages and never uninstalls them.

   ;; Deferred layer installation:
   dotspacemacs-enable-lazy-installation
   'unused ; (default) waits until opening a relevant file to install layers not listed in dotspacemacs-configuration-layers.
   ;; 'all ; waits until opening a relevant file to install layers that support deferred installation, even those listed in dotspacemacs-configuration-layers.
   ;; nil ; disables deferred installation.

   ;; Will Spacemacs ask before lazily installing layers?
   dotspacemacs-ask-for-lazy-installation t ; default t

   ;; Additional paths for configuration layers:
   ;; Paths must have a trailing slash (e.g. "~/.mycontribs/").
   dotspacemacs-configuration-layer-path '()

   ;; Configuration layers to install & load:
   dotspacemacs-configuration-layers
   '(
     (colors
      :packages
      rainbow-mode ; for color strings only
      :variables
      rainbow-x-colors nil
      rainbow-html-colors nil
      )
     ;; (org :variables org-enable-github-support t)
     (ranger :variables
      ranger-override-dired t
      ranger-show-preview t
      ranger-show-literal nil
      )
     (shell :variables
      shell-default-height 30
      shell-default-position 'bottom)
     spacemacs-completion
     spacemacs-editing
     (spacemacs-evil :packages
      (not vi-tilde-fringe)
      )
     spacemacs-ui ; includes restart-emacs
     ;;; Bindings:
     better-defaults
     vinegar ; dired
     ;;; Checking & Completion:
     auto-completion
     ivy
     (spell-checking :variables
      spell-checking-enable-by-default nil
      flyspell-sort-corrections nil
      )
     (syntax-checking :variables
      syntax-checking-enable-by-default nil
      )
     ;;; Languages:
     ;; (semantic :packages
     ;;           semantic
     ;;           srefactor)
     elm
     emacs-lisp
     haskell
     (html :variables ; for CSS ; this is called web-mode, not html-mode
      web-mode-css-indent-offset 2
      web-mode-enable-css-colorization nil ; already done with colors
      )
     javascript
     markdown
     python
     vimscript
     ;;; VC:
     git
     github
     version-control
     )

   ;; Packages installed & loaded without being wrapped in a layer:
   ;; If you need configuration for these packages, consider creating a layer. You can also put the configuration in dotspacemacs/user-config.
   dotspacemacs-additional-packages
   '(
     ;; Basic Libraries
     dash ; list functions
     dash-functional
     ;; Other Stuff
     ;; (acme-mouse :location (recipe :fetcher github :repo "akrito/acme-mouse")) ; does not work in Spacemacs.
     adaptive-wrap
     aggressive-indent
     company
     paren-face
     ;; (shen-elisp ; I have not been using shen-elisp.
     ;;  :location (recipe :repo "deech/shen-elisp"
     ;;                    :fetcher github
     ;;                    :files ("shen*.el"))
     ;;  :upgrade 't
     ;;  )
     )

   ;; Packages that will not be updated:
   dotspacemacs-frozen-packages '()

   ;; Packages and extensions that will not be installed or loaded:
   dotspacemacs-excluded-packages
   '(
     fancy-battery ; The GUI shell shows this.
     helm ; Use ivy instead.
     highlight-indentation ; Indentation shows this.
     highlight-parentheses ; Use paren-face-mode instead.
     orgit ; Doesn't fetch Org properly.
     powerline ; Use customized modeline instead.
     spray ; Not currently using spacemacs for speed reading.
     )
   ))

(defun dotspacemacs/init ()
  "Initialization function:
This function is called at the very startup of Spacemacs initialization before layers configuration. You should not put any user code in here besides modifying the variable values."
  ;;; Spacemacs settings:
  (setq-default
   ;;; ELPA
   ;; Will ELPA repositories be contacted via HTTPS?
   ;; Disable only if you have no way to use HTTPS. Launching Emacs with the parameter --insecure sets this variable to nil.
   dotspacemacs-elpa-https t ; default t

   ;; Maximum allowed time in seconds to contact an ELPA repository:
   dotspacemacs-elpa-timeout 5 ; default 5

   ;; Package subdirectory:
   ;; A form that evaluates to a directory. For example, to use different package directories for different Emacs versions, set this to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil ; default nil

   ;; The default package repository if no repository has been specified for an installed package:
   ;; Not used for now.
   dotspacemacs-default-package-repository nil ; default nil

   ;; Will Spacemacs check for updates at startup?
   ;; This is disabled when the current branch is develop.
   dotspacemacs-check-for-update nil ; default t

   ;; Editing style:
   dotspacemacs-editing-style
   'vim ; (default)
   ;; 'emacs
   ;; 'hybrid ; like vim except that insert state is replaced by the hybrid state with emacs key bindings.
   ;; The value can also be a list with :variables keyword. Check the editing styles section of the documentation for details on available variables.

   ;; Will Spacemacs output loading progress to the *Messages* buffer?
   dotspacemacs-verbose-loading nil ; default nil
   ;; Will Spacemacs display a progress bar when loading? This may increase the boot time.
   dotspacemacs-loading-progress-bar nil ; default t

   ;; The startup banner:
   dotspacemacs-startup-banner
   ;; 'official ; (default) displays the official spacemacs logo.
   ;; 'random ; chooses a random text banner in the core/banners directory.
   ;; 1 ; An integer is the index of a text banner.
   ;; "~/.emacs.d/assets/spacemacs.svg" ; A string must be a path to an image supported by your Emacs build.
   nil ; No banner is displayed.

   ;; Items to show in startup buffer:
   ;; A list or an association list of of the form `(list-type . list-size)`. If nil it is disabled. Possible values for list-type are: recents bookmarks projects agenda todos.
   dotspacemacs-startup-lists
   '((recents . 7)
     (projects . 7))

   ;; Will the startup buffer resize?
   dotspacemacs-startup-buffer-responsive t ; default t

   ;; Major mode of the scratch buffer:
   dotspacemacs-scratch-mode 'text-mode ; default 'text-mode

   ;; Themes:
   ;; The first of the list is loaded when spacemacs starts. Press <SPC> T n to cycle to the next theme in the list (works great with 2 themes variants, one dark and one light).
   dotspacemacs-themes
   (if (string= system-type "gnu/linux")
       '(
         spacemacs-dark ; dark theme
         leuven ; light theme
         )
     '(
       leuven ; light theme
       spacemacs-dark ; dark theme
       )
     )

   ;; Will the cursor color match the state color in GUI Spacemacs?
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font (or prioritized list of fonts):
   ;; `powerline-scale' provides tweaking of the mode-line size to make separators look better.
   dotspacemacs-default-font
   '(
     "Source Code Pro"
     :size 14.0
     :weight normal
     :width normal
     :powerline-scale 1.0
     )

   ;; The leader key:
   dotspacemacs-leader-key "SPC"
   ;; The command key used for Vim Ex commands (ex-commands):
   dotspacemacs-ex-command-key ":"
   ;; Major mode leader key:
   ;; Equivalent to '<leader> m'. Disabled when nil.
   dotspacemacs-major-mode-leader-key "," ; default ","
   ;; The leader key in emacs state and insert state:
   dotspacemacs-emacs-leader-key "M-m" ; default "M-m"
   ;; The key used for Emacs commands (M-x) after pressing on the leader key:
   dotspacemacs-emacs-command-key "SPC" ; default "SPC"
   ;; Major mode leader key accessible in emacs state and insert state:
   dotspacemacs-major-mode-emacs-leader-key "C-M-m" ; default "C-M-m"

   ;;; Vim keybindings
   ;; Will Y be remapped to `y$'?
   dotspacemacs-remap-Y-to-y$ t ; default t
   ;; Will the shift mappings < and > maintain visual state?
   dotspacemacs-retain-visual-state-on-shift t ; default t
   ;; Will J and K move lines up and down when in visual state?
   dotspacemacs-visual-line-move-text nil ; default nil
   ;; Will the meaning of g be inverted in :substitute Evil ex-commands?
   dotspacemacs-ex-substitute-global nil ; default nil

   ;; Variables to control whether separate commands are bound in the GUI to the key pairs C-i, TAB and C-m, RET:
   ;; Setting it to a non-nil value allows for separate commands under <C-i> and TAB or <C-m> and RET. In the terminal, these pairs are generally indistinguishable, so this only works in the GUI.
   dotspacemacs-distinguish-gui-tab nil ; default nil

   ;;; Layouts
   ;; Name of the default layout:
   dotspacemacs-default-layout-name "Default" ; default "Default"
   ;; Will the default layout name be displayed in the mode-line?
   dotspacemacs-display-default-layout nil ; default nil
   ;; Will the last auto saved layouts resume automatically on start?
   dotspacemacs-auto-resume-layouts nil ; default nil

   ;; Size (in MB) above which spacemacs will prompt to open a large file without modes to avoid performance issues:
   dotspacemacs-large-file-size 1 ; default 1

   ;; Where to auto-save files:
   dotspacemacs-auto-save-file-location
   'cache ; (default) auto-saves the file to another file stored in the cache directory.
   ;; 'original ; auto-saves the file in-place.
   ;; nil ; disables auto-saving.

   ;;; Maximum number of rollback slots to keep in the cache:
   dotspacemacs-max-rollback-slots 5 ; default 5

   ;;; Helm
   ;; Will `helm' will try to minimize its size?
   dotspacemacs-helm-resize nil ; default nil
   ;; Will the helm header be hidden when there is only one source?
   dotspacemacs-helm-no-header t ; default nil
   ;; The position of helm:
   ;; Options are bottom, top, left, or right.
   dotspacemacs-helm-position 'bottom ; default 'bottom

   ;; Fuzzy matching in helm:
   dotspacemacs-helm-use-fuzzy
   'always ; (default) force fuzzy matching in all non-asynchronous sources.
   ;; 'source ; preserve individual source settings.
   ;; nil ; disable fuzzy matching in all sources.

   ;; Paste micro-state:
   ;; Will 'p' cycle through the kill ring content?
   dotspacemacs-enable-paste-transient-state t ; default nil

   ;; Which-key popup delay in seconds:
   ;; The which-key buffer is a list of the commands sharing the current keystroke prefix.
   dotspacemacs-which-key-delay 0.4 ; default 0.4

   ;; Which-key frame position:
   dotspacemacs-which-key-position
   'bottom ; (default)
   ;; 'right
   ;; 'right-then-bottom ; tries to display the frame to the right; if there is insufficient space it is displayed at the bottom.

   ;; Should switch-to-buffer put the buffer in a same-purpose window even if the buffer can be put in the current window?
   dotspacemacs-switch-to-buffer-prefers-purpose t ; default nil

   ;;; Fullscreen
   ;; Will Spacemacs start up fullscreen? (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil ; default nil
   ;; Will spacemacs/toggle-fullscreen use non-native fullscreen? Use to disable fullscreen animations in OSX.
   dotspacemacs-fullscreen-use-non-native nil ; default nil
   ;; Will the frame be maximized when Spacemacs starts up? Ignored if dotspacemacs-fullscreen-at-startup is not nil. (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil ; default nil

   ;;; Frame opacity
   ;; Transparency can be toggled through toggle-transparency.
   ;; Active and selected frames:
   dotspacemacs-active-transparency 90 ; default 90
   ;; Inactive and unselected frames:
   dotspacemacs-inactive-transparency 90 ; default 90

   ;;; Transient states
   ;; Will the titles of transient states be shown?
   dotspacemacs-show-transient-state-title t ; default t
   ;; Will the color guide hint for transient state keys be shown?
   dotspacemacs-show-transient-state-color-guide t ; default t

   ;; Will Unicode symbols be displayed in the mode line?
   dotspacemacs-mode-line-unicode-symbols t ; default t

   ;; Smooth scrolling:
   ;; Smooth scrolling overrides the default behavior of Emacs, which re-centers the point when it reaches the top or bottom of the screen. t enables, nil disables.
   dotspacemacs-smooth-scrolling t ; default t

   ;; Line numbers:
   dotspacemacs-line-numbers
   nil ; (default) disables line numbers.
   ;; t ; turns on line numbers in all `prog-mode' and `text-mode' derivatives.
   ;; 'relative ; also turns on relative line numbers.
   ;; '(:relative nil :disabled-for-modes dired-mode doc-view-mode markdown-mode org-mode pdf-view-mode text-mode :size-limit-kb 1000) ; A property list can be used for finer control.


   ;; Code folding:
   dotspacemacs-folding-method
   'evil ; (default)
   ;; 'origami

   ;; Scope for highlighting delimiters:
   ;; Possible values are 'any', 'current', 'all' or nil. 'all' highlights any scope and emphasizes the current one.
   dotspacemacs-highlight-delimiters 'current ; default nil

   ;;; Smartparens
   ;; Will `smartparens-strict-mode' be enabled in `prog-mode'?
   dotspacemacs-smartparens-strict-mode nil ; default nil
   ;; Will `)' in insert mode pass over any automatically added closing parenthesis, bracket, quote, etc.? This can be temporarily disabled by pressing `C-q' before `)'.
   dotspacemacs-smart-closing-parenthesis nil ; default nil

   ;; Whitespace cleanup on save:
   dotspacemacs-whitespace-cleanup
   ;; nil ; disables cleanup (default).
   ;; 'all ; aggressively deletes empty lines and long sequences of whitespace.
   ;; 'trailing ; deletes only the whitespace at end of lines.
   'changed ; deletes whitespace only for changed lines.

   ;; Server:
   ;; Will quit functions be advised to leave the server running?
   dotspacemacs-persistent-server nil ; default nil

   ;; Search Tools:
   ;; Spacemacs uses the first installed tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep") ; default '("ag" "pt" "ack" "grep")
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
This function is called immediately after `dotspacemacs/init', before layer configuration. It is mostly useful for variables that must be set before packages are loaded. If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (defconst the-default-mode-line mode-line-format) ; Save in case you want to know.

  ;; (if (display-graphic-p)
  ;;     (progn
  ;;       (customize-set-variable 'adaptive-fill-first-line-regexp
  ;;                               "\\`[ \t]*\\'\\([*;]+\\)*")
  ;;       (customize-set-variable 'adaptive-fill-regexp
  ;;                               "[ \t]*\\([-–!|#%>·•‣⁃◦]+[ \t]*\\)*"))
  ;;   (progn
  ;;     (customize-set-variable 'adaptive-fill-first-line-regexp
  ;;                             "\\`[ \t]*\\'\\*")
  ;;     (customize-set-variable 'adaptive-fill-regexp
  ;;                             "[ \t]*\\([-–!|#%;>·•‣⁃◦]+[ \t]*\\)*")))
  ;; Removed ';' in graphic mode, since comments are indicated by text color. Removed '*' so I can make non-unicode bullet lists. Ideally there should be two separate variables: adaptive-fill-regexp and adaptive-indent-regexp. The first would indent with the 'whitespace' character, but the second would indent with actual whitespace.
  ;; Default `adaptive-fill-regexp': [ \t]*\\([-–!|#%;>*·•‣⁃◦]+[ \t]*\\)*".
  ;; Default `adaptive-fill-first-line-regexp': "\\`[ \t]*\\'".
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization, after layers configuration. Put your configuration code--except for variables that should be set before a package is loaded--here."

;;;; Indent forms properly
  ;; (defun use-cl-indent (PROCEDURE INDENT-PROPERTY)
  ;;   (function-put PROCEDURE
  ;;                 'lisp-indent-function
  ;;                 #'common-lisp-indent-function)
  ;;   (function-put PROCEDURE
  ;;                 'common-lisp-indent-function-for-elisp
  ;;                 INDENT-PROPERTY))

  ;; (use-cl-indent 'cl-flet '((&whole 4 &rest (&whole 1 &lambda &body)) &body))
  ;; This doesn't seem to work, so just set lisp-indent-function.
  (setq lisp-indent-function #'common-lisp-indent-function)

;;;; Helpful Procedures

  (defun my-eval-args (PROCEDURE &rest ARGS)
    "Evaluate ARGS before applying PROCEDURE to them (in a lexical context)."
    (declare (indent defun))
    (eval `(,PROCEDURE ,@ARGS) t))

  (defun my-customize-set-variables (&rest ASSOCS)
    "Takes zero or more (SYMBOL . VALUE) arguments and customizes SYMBOL to VALUE."
    (dolist (key.val ASSOCS)
      (customize-set-variable (car key.val) (cdr key.val))))

;;; Hooks:

  (defun my-make-hook (WHEN PROCEDURE &optional CONTINGENT)
    "Create hook WHEN-PROCEDURE-hook to run WHEN PROCEDURE is called, unless it is already defined. The CONTINGENT functions are added to the hook regardless."
    (let* ((when-str (substring (symbol-name WHEN) 1))
           (proc-name (symbol-name PROCEDURE))
           (hook-name (concat when-str "-" proc-name "-hook"))
           (existing-hook (intern-soft hook-name))
           (hook-symbol (or existing-hook (intern hook-name))))
      (unless existing-hook
        (set hook-symbol nil)
        (set-default hook-symbol nil) ; Should not need this, because if the hook doesn't exist it can't be buffer-local.
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

  (defun my-hook-up (HOOKS FUNCTIONS)
    "Hang all FUNCTIONS, in order, on all HOOKS."
    (dolist (hook HOOKS)
      (dolist (function (reverse FUNCTIONS))
        (add-hook hook function))))

;;; Buffers and Panes

  (defmacro my-with-buffer (BUFFER &rest BODY)
    "If BUFFER is not nil, execute BODY in BUFFER. Otherwise, execute BODY (in the current buffer)."
    (declare (indent 1))
    `(save-current-buffer
       (and ,BUFFER (set-buffer ,BUFFER))
       ,@BODY))

  ;;; Track primary pane.
  (defvar my-primary-pane (frame-selected-window)
    "The pane that has an active mode-line.")

  (defun my-set-primary-pane ()
    "Set the primary pane."
    (let ((p (frame-selected-window)))
      (unless (minibuffer-window-active-p p)
        (setq my-primary-pane p))))

  (defun my-primary-pane-active? ()
    (eq my-primary-pane (selected-window)))

  (my-make-hook :after 'select-frame)
  (my-make-hook :after 'handle-select-window)

  (my-hook-up
   '(
     after-select-frame-hook
     after-handle-select-window-hook
     buffer-list-update-hook
     focus-in-hook
     window-configuration-change-hook
     )
   '(my-set-primary-pane))

  (defvar-local my-buffer-line-count nil)

  (defun my-buffer-line-count (&optional BUFFER)
    "Number of lines in the current buffer. If the last line of the buffer is empty, it won't be counted."
    (my-with-buffer BUFFER
      (count-lines (buffer-end -1) (buffer-end 1))))

  (defun my-set-buffer-line-count (&rest _)
    (set 'my-buffer-line-count (my-buffer-line-count)))

  (my-hook-up
   '(
     buffer-list-update-hook
     after-change-functions
     )
   '(my-set-buffer-line-count))

  (defun my-buffer-file-like-p (&optional BUFFER)
    "Is the buffer visiting something that should be a file?"
    (my-with-buffer BUFFER
      (or buffer-file-name
          (derived-mode-p 'prog-mode 'text-mode))))

  ;;; Buffer and File Names

  (defun my-buffer-file-path (&optional BUFFER)
    "The file path if BUFFER is a file, otherwise nil. If BUFFER is nil, use the current buffer."
    (let ((file (buffer-file-name BUFFER)))
      (when file (abbreviate-file-name (file-truename file)))))

  (defun my-primary-file-or-buffer-name ()
    "The name of the file or buffer in the primary pane."
    (let ((b (window-buffer my-primary-pane)))
      (or (my-buffer-file-path b)
          (buffer-name b))))

  ;; (defvar-local my-file-VC-status nil
  ;;   "The version-control status of the current file.")
  ;; (defun my-file-VC-status (&optional FILE)
  ;;   "The version-control status of FILE or the file visited by the current buffer."
  ;;   (let ((f (or FILE (my-buffer-file-path))))
  ;;     (and f (vc-state f))))
  ;; (defun my-set-file-VC-status (&rest _)
  ;;   "Set the buffer-local variable `my-file-VC-status' to the version-control status of the file visited by the current buffer."
  ;;   (set 'my-file-VC-status (my-file-VC-status)))

  ;; ;; Ways `magit' can run git:
  ;; ;; `magit-start-process'
  ;; ;; `magit-call-process'
  ;; ;; `magit-start-git' uses `magit-start-process'.
  ;; ;; `magit-call-git' uses `magit-call-process'.
  ;; ;; `magit-run-git-async' uses `magit-start-git'.
  ;; ;; `magit-run-git-with-input' uses `magit-start-git' and `call-process-region'.
  ;; ;; `magit-git' uses `magit-call-git'.
  ;; ;; `magit-run-git' uses `magit-call-git'
  ;; ;; `magit-run-git-with-editor' uses `magit-run-git-async'.
  ;; ;; `magit-run-git-with-logfile' uses `magit-process-file'.
  ;; ;; `magit-git-wash'

  ;; (my-hook-up
  ;;  '(
  ;;    after-save-hook
  ;;    find-file-hook
  ;;    first-change-hook
  ;;    )
  ;;  '(my-set-file-VC-status))

  ;; (defun my-file-VC-status-string ()
  ;;   "A string that represents the VC status of the file visited by the current buffer."
  ;;   (pcase my-file-VC-status
  ;;     (`up-to-date "")
  ;;     (`ignored "")
  ;;     (`edited "◆")
  ;;     (`needs-update "U")
  ;;     (`needs-merge "M")
  ;;     (`added "+")
  ;;     (`removed "-")
  ;;     (`conflict "!")
  ;;     (`missing "?")
  ;;     (_ nil)))

;;; Numbers:

  (defun my-digits (N)
    "Number -> Integer
The number of decimal digits of N, including any period as a digit."
    (length (number-to-string N)))

;;; Strings:

  (defun my-pad (W S)
    "Integer -> String -> String
Pad string S with spaces to width W. A negative width means add the padding on the right."
    (format (concat "%" (number-to-string W) "s") S))

;;; Colors

  (defun max-color-val ()
    "The current maximum value for emacs color triplets."
    (car (color-values "white")))

  (defun my-color-values->string (C)
    "(R G B) -> String
Create a color string from and Emacs numerical color triplet."
    ;; Normalize to (0, 255).
    (-let* ((ratio (/ (max-color-val) 255))
            ((r g b) (mapcar (lambda (x) (truncate x ratio))
                             C)))
      (format "#%02X%02X%02X" r g b)))

  (defun my-blend-colors (C1 C2)
    "(R G B) -> (R G B) -> (R G B)
Evenly blend C1 and C2, two emacs color triplets."
    (-zip-with (lambda (X Y) (truncate (+ X Y) 2))
               C1 C2))

  (defun my-intensify-color (COLOR REFERENCE)
    "(R G B) -> (R G B) -> (R G B)
Shift COLOR away from REFERENCE."
    (my-blend-colors COLOR
                     (color-values (if (> (-sum COLOR)
                                          (-sum REFERENCE))
                                       "white"
                                     "black"))))

;;; Fonts & Faces

  (defun my-select-font (FONTS)
    "Return the first available font in FONTS, or the default font if none are available."
    (cond ((null FONTS) (face-attribute 'default :family))
          ((member (car FONTS) (font-family-list)) (car FONTS))
          (t (my-select-font (cdr FONTS)))))

  (defun my-def-faces (GROUP &rest FACES)
    "Create FACES (name docstring properties) in GROUP. No fancy business here; the display is always t."
    (declare (indent 1))
    (dolist (face FACES)
      (-let [(name docstring . properties) face]
        (custom-declare-face name (list (cons t properties)) docstring :group GROUP))))

  (defun my-set-face-attributes (L &optional BUFFER)
    "From list L of (face :attr-1 a1 :attr-2 a2 ...) lists, give each face its attributes. Create undefined faces."
    (dolist (x L)
      (-let [(face . attributes) x]
        (unless (facep face) (make-face face))
        (apply 'set-face-attribute face BUFFER attributes))))

  (defun my-fade-face-foreground (FACE REFERENCE)
    "Make FACE's foreground a less intense version of REFERENCE's.
REFERENCE is used to avoid fading FACE into oblivion with repreated applications."
    (cl-flet
        ((color-of (KEY)
           (color-values (face-attribute REFERENCE KEY nil 'default))))
      (set-face-attribute
       FACE
       nil
       :foreground (my-color-values->string
                    (my-blend-colors (color-of :foreground)
                                     (color-of :background))))))

  ;;; ----------------------------------------------
  ;;; Mode Line, Header Line, and Frame Title Format

  (my-def-faces 'statusbar
    '(my-statusbar-active-face "an alias for mode-line face" :inherit mode-line)
    '(my-statusbar-inactive-face "an alias for mode-line-inactive face" :inherit mode-line-inactive)
    '(my-statusbar-active-highlight-face "an emphasized face for the active mode-line" :inherit my-statusbar-active-face)
    '(my-statusbar-inactive-highlight-face "an emphasized face for the inactive mode-line" :inherit my-statusbar-inactive-face)
    '(my-statusbar-active-shadow-face "a dimmed face for the active mode-line" :inherit my-statusbar-active-face)
    '(my-statusbar-inactive-shadow-face "a dimmed face for the inactive mode-line" :inherit my-statusbar-inactive-face)
    )

  (defun my-get-statusbar-face ()
    "an ersatz face that switches between statusbar-active- and statusbar-inactive-face"
    (if (my-primary-pane-active?)
        'my-statusbar-active-face
      'my-statusbar-inactive-face))

  (defun my-get-statusbar-shadow-face ()
    "an ersatz face that switches between statusbar-active- and statusbar-inactive-shadow-face"
    (if (my-primary-pane-active?)
        'my-statusbar-active-shadow-face
      'my-statusbar-inactive-shadow-face))

  (defun my-reset-statusbar-faces ()
    "Set statusbar shadow faces to be faded versions of their counterparts."
    (interactive)
    (my-fade-face-foreground
     'my-statusbar-active-shadow-face
     'my-statusbar-active-face)
    (my-fade-face-foreground
     'my-statusbar-inactive-shadow-face
     'my-statusbar-inactive-face))
  (my-reset-statusbar-faces)

  (my-make-hook :after 'load-theme)
  (add-hook 'after-load-theme-hook 'my-reset-statusbar-faces)

  (defun my-buffer-name ()
    "The name of the buffer. If it's a file, show the directory on hover and open dired with a click."
    (if buffer-file-truename
        (propertize (buffer-name)
                    'help-echo (abbreviate-file-name buffer-file-truename)
                    'local-map (make-mode-line-mouse-map
                                'mouse-1 (lambda () (interactive)
                                                 (dired (file-name-directory buffer-file-truename)))))
      (buffer-name)))

  (defun my-major-mode-name ()
    "The buffer's major-mode"
    (propertize mode-name
                'help-echo "Click mouse 1 for mode menu, mouse 2 for mode info, or mouse 3 to toggle minor modes."
                'local-map mode-line-major-mode-keymap))

  (defun my-buffer-write-status ()
    "Show whether a file-like buffer has been modified since its last save; click to save. Should 'do what I mean'."
    (if (not (my-buffer-file-like-p))
        "" ; Ignore buffers that aren't files.
      (my-pad 1 (propertize
                 (concat (when (buffer-modified-p) "◆")
                         (when buffer-read-only "🔒"))
                 'help-echo
                 (concat (when (buffer-modified-p) "modified ")
                         (when buffer-read-only "read-only ")
                         (if buffer-file-name "file " "buffer ")
                         "‑ click to save")
                 'local-map (make-mode-line-mouse-map 'mouse-1 #'save-buffer)))))

  (defun my-vc-branch ()
    (if (not vc-mode)
        ""
      (concat
       (propertize "(" 'face (my-get-statusbar-shadow-face))
       (propertize
        (replace-regexp-in-string " Git[:\-]" "" vc-mode)
        'mouse-face (my-get-statusbar-face)
        'local-map (make-mode-line-mouse-map 'mouse-1 #'magit-status))
       (propertize ")" 'face (my-get-statusbar-shadow-face))
       )))

  (defun my-line-position ()
    "Current line / total lines. Click to toggle line numbers."
    (let ((lines (number-to-string my-buffer-line-count)))
      (propertize
       (concat
        (my-pad (length lines) (format-mode-line "%l"))
        (propertize "/" 'face (my-get-statusbar-shadow-face))
        lines)
       'help-echo (if (bound-and-true-p linum-mode) "Hide line numbers." "Show line numbers.")
       'local-map (make-mode-line-mouse-map 'mouse-1 #'linum-mode))))

;;; TODO: Create shortened mode-line faces for a collapsed but visible mode line.

  (defvar my-base-mode-line-format
    '(:eval
      (concat
       (my-buffer-write-status)
       " "
       (my-buffer-name)
       " "
       (my-vc-branch)
       "  "
       (my-line-position)
       ))
    "a simple status bar")

  (setq-default
   mode-line-format my-base-mode-line-format)

  (defun my-format-text-mode-line ()
    (setq mode-line-format my-base-mode-line-format))

  (defvar my-prog-mode-line-format
    (list
     my-base-mode-line-format
     '(:eval
       (concat
        "  "
        (my-major-mode-name)
        "  "
        (when (bound-and-true-p anzu-mode) (anzu--update-mode-line))
        )))
    "simple status bar that indicates the current mode")

  (defun my-format-prog-mode-line ()
    (setq mode-line-format my-prog-mode-line-format))

  (defun my-format-frame-title ()
    (when (display-graphic-p)
      (setq frame-title-format
            '(:eval (concat
                     (my-buffer-write-status)
                     " "
                     (my-primary-file-or-buffer-name)
                     )))))
  (my-format-frame-title)

  ;;; --------------------------------
  ;;; Fonts

  (set-face-attribute
   'fixed-pitch nil
   :family (my-select-font
            '(
              "Source Code Pro"
              "IBM 3720"
              "DejaVu Sans Mono"
              "Monaco"
              "Lucida Console"
              )))

  (set-face-attribute
   'variable-pitch nil
   :family (my-select-font
            '(
              "ET Book"
              "ETBembo"
              "Bembo Book MT Std"
              "Bembo MT Book Std"
              "Garamond Premier Pro"
              "Garamond Premr Pro"
              "Adobe Garamond Expert"
              "Garamond"
              )))

  (defun my-reset-font-height-by-platform ()
    "Make the font bigger if running linux, because my laptop runs linux and my desktop runs Windows."
    (let ((h (if (string= system-type "gnu/linux") 148 120)))
      (dolist (face '(
                      default
                      fixed-pitch
                      variable-pitch
                      ))
        (set-face-attribute face nil :height h))))

  (my-hook-up
   '(
     after-load-theme-hook
     window-setup-hook
     )
   '(my-reset-font-height-by-platform))

  ;;; ---------------------------------
  ;;; Miscelaneous Global Stuff

  (global-hl-line-mode -1) ; Disable current line highlight.
  (global-visual-line-mode 1) ; Always wrap lines to window.
  (setq-default major-mode 'text-mode) ; Use text instead of fundamental.
  (setq vc-follow-symlinks t)

  ;;; ----------------------------------
  ;;; Set Evil to not behave like Vim.
  ;; commented out in case it's causing errors.
  ;; (customize-set-variable 'evil-move-beyond-eol t) ; Allow the cursor to move beyond the end of the line.
  ;; (customize-set-variable 'evil-move-cursor-back nil) ; Don't move the cursor when exiting insert mode.

  ;; ;; Flip Vi a/A behavior.
  ;; (define-key evil-normal-state-map "a" 'evil-append-line)
  ;; (define-key evil-normal-state-map "A" 'evil-append)

  ;; (defun my-evil-forward-end (THING &optional COUNT)
  ;;   "Move forward past the end of THING. Repeat COUNT times."
  ;;   ;; (unless (eobp) (forward-char))
  ;;   (forward-thing THING (or COUNT 1)))

  ;; (evil-define-motion my-evil-forward-word-end (COUNT &optional BIGWORD)
  ;;   "Move the cursor past the end of the COUNT-th next word."
  ;;   :type 'inclusive ; I don't know what this does!
  ;;   (let ((thing (if BIGWORD
  ;;                    'evil-WORD
  ;;                  'evil-word))
  ;;         (n (or COUNT 1)))
  ;;     (evil-signal-at-bob-or-eob n)
  ;;     (my-evil-forward-end thing n)))

  ;; (evil-define-motion my-evil-forward-WORD-end (COUNT)
  ;;   "Move the cursor past the end of the COUNT-th next bigword."
  ;;   :type 'exclusive ; I don't know what this does!
  ;;   (my-evil-forward-word-end COUNT t))

  ;; ;; Flip Vi e/E behavior to make a more useful distiction from w/W.
  ;; (define-key evil-motion-state-map "E" 'my-evil-forward-word-end)
  ;; (define-key evil-motion-state-map "e" 'my-evil-forward-WORD-end)

  ;;; ----------------------------------
  ;;; Hooks

  (my-hook-up
   '(
     after-change-major-mode-hook
     after-save-hook
     buffer-list-update-hook
     first-change-hook
     )
   '(force-mode-line-update))

  ;; Refresh VC state to update mode line info. Fall back to expensive vc-find-file-hook if `vc-refresh-state' is not available.

  (my-make-hook :after 'magit-run-git)

  (my-make-hook :after 'magit-start-process)

  (my-hook-up
   '(
     after-magit-run-git-hook
     after-magit-start-process-hook
     )
   `(
     ,(if (fboundp 'vc-refresh-state) 'vc-refresh-state 'vc-find-file-hook)
      ,(lambda () (force-mode-line-update t)) ; refresh all mode lines.
      ))

  (my-hook-up
   '(prog-mode-hook)
   '(
     adaptive-wrap-prefix-mode ; Indent wrapped lines in source code.
     rainbow-mode ; Color color strings like "#4971af" in source code.
     my-format-prog-mode-line
     ))

  (my-hook-up
   '(text-mode-hook)
   '(
     variable-pitch-mode
     my-format-text-mode-line
     ))

  (with-current-buffer "*Messages*" (my-format-text-mode-line)) ; Use a simple mode line for the *Messages* buffer.

  ;; Hide the mode-line when not needed useful.
  (my-hook-up
   '(
     help-mode-hook
     magit-mode-hook
     ranger-mode-hook
     spacemacs-buffer-mode-hook
     )
   '((lambda () (setq mode-line-format nil))))

  ;;; ------------------------------
  ;;; Key Maps

  (unless (string= system-type "gnu/linux")
    (define-key help-mode-map
        (kbd "<mouse-4>") 'help-go-back) ; mouse forwards
    (define-key help-mode-map
        (kbd "<mouse-5>") 'help-go-forward)) ; mouse back

  ;; Navigate wrapped lines.
  (define-key evil-normal-state-map
      (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map
      (kbd "k") 'evil-previous-visual-line)

  ;; Paste with Ctrl p.
  (define-key evil-insert-state-map
      (kbd "C-p") 'evil-paste-after)

  ;; Insert unicode character with Ctrl Shift u.
  (defun my-ivy-prefix-sort (name candidates)
    "Re-sort CANDIDATES.
Prefix matches to NAME are put ahead of the list, with the shortest matches first."
    ;; What I want it to push exact matches to the top.
    (if (or (string-match "^\\^" name) (string= name ""))
        candidates
      (let ((re-prefix (concat "^" (funcall ivy--regex-function name)))
            res-prefix
            res-noprefix)
        (dolist (s candidates)
          (if (string-match re-prefix s)
              (push s res-prefix)
            (push s res-noprefix)))
        (nconc
         res-prefix
         (nreverse res-noprefix)))))
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-unicode-char . my-ivy-prefix-sort))

  (defun my-ivy-compare-strings-by-length (S1 S2)
    "Compare first by length (shortest first), then alphabetically (case insensitive)"
    (let ((L1 (length S1))
          (L2 (length S2)))
      (or (< L1 L2) (> L1 L2) (string-collate-lessp S1 S2 nil t))))
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-unicode-char . my-ivy-compare-strings-by-length))

  (global-set-key
   (kbd "C-S-u") 'counsel-unicode-char) ; `counsel-unicode-char' is slow...

  ;; (evil-define-command my-greedy-delete-backward () ;; commented out because it was bugging things.
  ;;   (evil-delete (save-excursion
  ;;                  (evil-backward-word-begin)
  ;;                  (point))
  ;;                (point)
  ;;                'exclusive
  ;;                nil)
  ;;   (delete-horizontal-space t))

  ;; (define-key evil-insert-state-map
  ;;   (kbd "<backspace>") 'my-greedy-delete-backward) ; Make backspace delete the whole word.


  ;; Zoom with Ctrl + mouse wheel.
  (defun my-zoom-in ()
    (interactive)
    (text-scale-increase 1.01))
  (defun my-zoom-out ()
    (interactive)
    (text-scale-decrease 1.01))
  (dolist (x '(
               (my-zoom-in "C-<mouse-4>" "C-<wheel-up>")
               (my-zoom-out "C-<mouse-5>" "C-<wheel-down>")
               ))
    (global-set-key
     (kbd (if (string= system-type "gnu/linux")
              (cadr x)
            (caddr x)))
     (car x)))

  ;;; Use hex for unicode character input.
  (setq-default read-quoted-char-radix 16)

  ;;; Mouse & copy / paste / delete
  (setq
   ;; mouse-drag-copy-region t ; Copy on select -- disable for acme-mouse.
   delete-selection-mode t ; Allow typing over the selection.
   kill-do-not-save-duplicates t ; Don't copy identical text twice.
   )

  ;;; -------------------------
  ;;; Major Mode Configurations

  ;;; Git
  ;; Use spacemacs for editing git commit messages.
  (global-git-commit-mode t)

  ;;; Elisp:
  (my-hook-up
   '(emacs-lisp-mode-hook)
   '(
     aggressive-indent-mode
     paren-face-mode ; Fade parentheses.
     ))

  ;;; Markdown:
  (add-hook 'markdown-mode-hook
            (lambda ()
              (set-face-attribute 'markdown-pre-face
                                  nil
                                  :family (face-attribute 'fixed-pitch :family))))
  ;;; Elm:
  ;; (defun my-elm-mode-hook ()
  ;;   "elm setup adapted from http://www.lambdacat.com/post-modern-emacs-setup-for-elm/"
  ;;   (add-to-list 'company-backends '(company-elm))
  ;;   (elm-oracle-setup-completion))
  ;; (add-hook 'elm-mode-hook 'my-elm-mode-hook)

  ;;; Sh
  (add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

  ;;; ---------------------------------------
  ;;; Lastly, some hackish theming:
  ;;; The main point is to, as much as possible without being distracting, distinguish stuff that does stuff from stuff that does not do stuff and things that look similar and act differently.

  (defun my-box->lines (FACE)
    (let ((color
           (pcase (face-attribute FACE :box)
             (`nil nil)
             (`t (face-attribute 'default :color))
             ((and (pred stringp) c) c)
             (plist (plist-get plist :color)))))
      (when color (set-face-attribute
                   FACE nil :box nil :underline color :overline color))))

  (defun my-laser-minor-theme (&optional COLOR)
    "Add borders to the mode-line and disable its background color."
    (interactive)
    (let ((c (if COLOR COLOR
               (face-attribute 'my-statusbar-active-face :foreground nil 'default))))
      (my-set-face-attributes
       `(
         (mode-line
          :box nil
          :foreground unspecified
          :background unspecified
          :underline ,c
          :overline ,c
          :inherit font-lock-comment-face)
         (window-divider :foreground ,c)
         ))))

  (defun my-material-minor-theme ()
    "Remove borders from the mode-line when its background is different from the buffer's."
    (interactive)
    (cl-flet
        ((unbox (FACE)
           (unless (equal
                    (face-attribute 'default :background)
                    (face-attribute FACE :background nil 'default))
             (set-face-attribute
              FACE nil
              :box nil
              :underline nil
              :overline nil))))
      (unbox 'mode-line)
      (unbox 'mode-line-inactive)))

  (defun my-theme-tweaks ()
    "Tweak faces to simplify themes."
    (my-set-face-attributes
     `(
       ;;; Things that don't do stuff:
       (font-lock-comment-face
        :background unspecified
        :slant normal)
       (font-lock-doc-face
        :inherit font-lock-comment-face)
       (fringe
        :background unspecified
        :foreground unspecified
        :inherit font-lock-comment-face)
       (linum
        :background unspecified
        :foreground unspecified
        :inherit font-lock-comment-face)
       ;;; Things that do stuff:
       (font-lock-keyword-face
        :foreground unspecified
        :inherit default)
       (font-lock-function-name-face
        :foreground unspecified
        :inherit default)
       (font-lock-variable-name-face
        :foreground unspecified
        :inherit default)
       ;;; Things that look like other things:
       (font-lock-string-face :slant italic)
       ))
    (my-fade-face-foreground 'shadow 'default)
    (my-fade-face-foreground 'font-lock-comment-delimiter-face 'font-lock-comment-face)
    (my-box->lines 'mode-line)
    (my-box->lines 'mode-line-inactive)
    (my-material-minor-theme))
  (my-theme-tweaks)
  (add-hook 'after-load-theme-hook #'my-theme-tweaks)

  )

;; Do not write anything past this comment. This is where Emacs will auto-generate custom variable definitions (I wish it wouldn't).
