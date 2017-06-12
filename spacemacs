;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; This file is loaded by Spacemacs at startup. It must be stored in your home directory.
;; FIXME: `gui-get-primary-selection' tries to activate on entering this buffer and fails with a message.
;; FIXME: `emacs-lisp' layer pops up an args out of bounds error on save.
;; FIXME: Prevent emacs from pasting contents of clipboard on entering buffer.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
   dotspacemacs-ask-for-lazy-installation t

   ;; Additional paths for configuration layers:
   ;; Paths must have a trailing slash (e.g. "~/.mycontribs/").
   ;; FIXME: Should be named `-paths'.
   dotspacemacs-configuration-layer-path '()

   ;; Configuration layers to install & load:
   dotspacemacs-configuration-layers
   '(
     (colors :packages
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
     ;; (shell :variables
     ;;  shell-default-height 30
     ;;  shell-default-position 'bottom)
     spacemacs-completion
     spacemacs-editing
     (spacemacs-evil :packages
                     (not vi-tilde-fringe)
                     )
     spacemacs-navigation ; includes restart-emacs
     ;; ;;; Bindings:
     ;; better-defaults
     ;; vinegar ; dired
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
     ;; ;; (semantic :packages
     ;; ;;           semantic
     ;; ;;           srefactor)
     ;; elm
     emacs-lisp
     ;; haskell
     (html :variables ; for CSS ; this is called web-mode, not html-mode
           web-mode-css-indent-offset 2
           web-mode-enable-css-colorization nil ; already done with colors
           )
     (java :variables
           eclim-eclipse-dirs (if (string= system-type "gnu/linux")'("Ix/k/Programs/eclipse/current"))
           eclim-executable (if (string= system-type "gnu/linux") "/Ix/k/Programs/eclipse/eclim/current/")
           eclimd-default-workspace (if (string= system-type "gnu/linux") "/Ix/k/Files/Documents/Code/Java/workspaces/default")
           )
     ;; javascript
     ;; markdown
     ;; python
     ;; vimscript
     ;;; VC:
     git
     ;; github ; seems broken
     version-control
     )

   ;; Packages installed & loaded without being wrapped in a layer:
   ;; If you need configuration for these packages, consider creating a layer. You can also put the configuration in dotspacemacs/user-config.
   dotspacemacs-additional-packages
   `(
     ;;; Basic Libraries
     dash ; list functions
     ;; dash-functional
     ;;; Other Stuff
     (android-mode :variables
                   android-mode-sdk-dir (if (string= system-type "gnu/linux") "/Ix/k/Programs/android-sdk-tools"))
     ;; (acme-mouse :location (recipe :fetcher github :repo "akrito/acme-mouse")) ; does not work in Spacemacs.
     adaptive-wrap
     ;; aggressive-indent
     ;; company
     paren-face
     (shen-elisp :location
                 (recipe :repo "deech/shen-elisp"
                         :fetcher github
                         :files ("shen*.el"))
                 :upgrade t
                 )
     shen-mode
     ;; inf-shen
     )

   ;; Packages that will not be updated:
   dotspacemacs-frozen-packages '()

   ;; Packages and extensions that will not be installed or loaded:
   dotspacemacs-excluded-packages
   '(
     helm ; Use ivy instead. ; but this might be messing with initialization...
     highlight-indentation ; Indentation shows this.
     highlight-parentheses ; Use paren-face-mode instead.
     orgit ; Doesn't fetch Org properly.
     powerline ; Use customized modeline instead.
     )
   ))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;;; Spacemacs settings:
  (setq-default
   ;;; ELPA
   ;; Will ELPA repositories be contacted via HTTPS?
   ;; Disable only if you have no way to use HTTPS. Launching Emacs with the parameter --insecure sets this variable to nil.
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository:
   dotspacemacs-elpa-timeout 5

   ;; Will Spacemacs check for updates at startup?
   ;; This is disabled when the current branch is develop.
   dotspacemacs-check-for-update nil

   ;; Package subdirectory:
   ;; A form that evaluates to a directory. For example, to use different package directories for different Emacs versions, set this to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil

   ;; The default package repository if no repository has been specified for an installed package:
   ;; Not used for now.
   dotspacemacs-default-package-repository nil

   ;; Editing style:
   dotspacemacs-editing-style
   'vim ; (default)
   ;; 'emacs
   ;; 'hybrid ; like vim except that insert state is replaced by the hybrid state with emacs key bindings.
   ;; The value can also be a list with :variables keyword. Check the editing styles section of the documentation for details on available variables.

   ;; Will Spacemacs output loading progress to the *Messages* buffer?
   dotspacemacs-verbose-loading t

   ;; Will Spacemacs display a progress bar when loading? This may increase the boot time.
   dotspacemacs-loading-progress-bar nil

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
   dotspacemacs-startup-buffer-responsive t

   ;; Major mode of the *scratch* buffer:
   dotspacemacs-scratch-mode 'emacs-lisp-mode ; default 'text-mode

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
       ))

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

   ;; The key used for Emacs commands (M-x) after pressing on the leader key:
   dotspacemacs-emacs-command-key "SPC"

   ;; The command key used for Vim Ex commands (ex-commands):
   dotspacemacs-ex-command-key ":"

   ;; The leader key in emacs state and insert state:
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key:
   ;; Equivalent to '<leader> m'. Disabled when nil.
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in emacs state and insert state:
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; Variables to control whether separate commands are bound in the GUI to the key pairs C-i, TAB and C-m, RET:
   ;; Setting it to a non-nil value allows for separate commands under <C-i> and TAB or <C-m> and RET. In the terminal, these pairs are generally indistinguishable, so this only works in the GUI.
   dotspacemacs-distinguish-gui-tab nil

   ;;; Vim keybindings
   ;; Will Y be remapped to `y$'?
   dotspacemacs-remap-Y-to-y$ t

   ;; Will the shift mappings < and > maintain visual state?
   dotspacemacs-retain-visual-state-on-shift t

   ;; Will J and K move lines up and down when in visual state?
   dotspacemacs-visual-line-move-text nil

   ;; Will the meaning of g be inverted in :substitute Evil ex-commands?
   dotspacemacs-ex-substitute-global nil

   ;;; Layouts
   ;; Name of the default layout:
   dotspacemacs-default-layout-name "Default"

   ;; Will the default layout name be displayed in the mode-line?
   dotspacemacs-display-default-layout nil

   ;; Will the last auto saved layouts resume automatically on start?
   dotspacemacs-auto-resume-layouts nil

   ;; Size (in MB) above which spacemacs will prompt to open a large file without modes to avoid performance issues:
   dotspacemacs-large-file-size 1

   ;; Where to auto-save files:
   dotspacemacs-auto-save-file-location
   'cache ; (default) auto-saves the file to another file stored in the cache directory.
   ;; 'original ; auto-saves the file in-place.
   ;; nil ; disables auto-saving.

   ;;; Maximum number of rollback slots to keep in the cache:
   dotspacemacs-max-rollback-slots 5

   ;;; Helm
   ;; Will `helm' will try to minimize its size?
   dotspacemacs-helm-resize nil

   ;; Will the helm header be hidden when there is only one source?
   dotspacemacs-helm-no-header t

   ;; The position of helm:
   ;; Options are bottom, top, left, or right.
   dotspacemacs-helm-position 'bottom

   ;; Fuzzy matching in helm:
   dotspacemacs-helm-use-fuzzy
   'always ; (default) force fuzzy matching in all non-asynchronous sources.
   ;; 'source ; preserve individual source settings.
   ;; nil ; disable fuzzy matching in all sources.

   ;; Paste micro-state:
   ;; Will 'p' cycle through the kill ring content?
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key popup delay in seconds:
   ;; The which-key buffer is a list of the commands sharing the current keystroke prefix.
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position:
   dotspacemacs-which-key-position
   'bottom ; (default)
   ;; 'right
   ;; 'right-then-bottom ; tries to display the frame to the right; if there is insufficient space it is displayed at the bottom.

   ;; Should switch-to-buffer put the buffer in a same-purpose window even if the buffer can be put in the current window?
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;;; Fullscreen
   ;; Will Spacemacs start up fullscreen? (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; Will spacemacs/toggle-fullscreen use non-native fullscreen? Use to disable fullscreen animations in OSX.
   dotspacemacs-fullscreen-use-non-native nil

   ;; Will the frame be maximized when Spacemacs starts up? Ignored if dotspacemacs-fullscreen-at-startup is not nil. (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;;; Frame opacity
   ;; Transparency can be toggled through toggle-transparency.
   ;; Active and selected frames:
   dotspacemacs-active-transparency 90
   ;; Inactive and unselected frames:
   dotspacemacs-inactive-transparency 90

   ;;; Transient states
   ;; Will the titles of transient states be shown?
   dotspacemacs-show-transient-state-title t

   ;; Will the color guide hint for transient state keys be shown?
   dotspacemacs-show-transient-state-color-guide t

   ;; Will Unicode symbols be displayed in the mode line?
   dotspacemacs-mode-line-unicode-symbols t

   ;; Smooth scrolling:
   ;; Smooth scrolling overrides the default behavior of Emacs, which re-centers the point when it reaches the top or bottom of the screen. t enables, nil disables.
   dotspacemacs-smooth-scrolling t

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
   ;; Possible values are 'any, 'current, 'all or nil. 'all highlights any scope and emphasizes the current one.
   dotspacemacs-highlight-delimiters 'current ; default nil

   ;;; Smartparens
   ;; Will `smartparens-strict-mode' be enabled in `prog-mode'?
   dotspacemacs-smartparens-strict-mode nil ; default nil
   ;; Will `)' in insert mode pass over any automatically added closing parenthesis, bracket, quote, etc.? This can be temporarily disabled by pressing `C-q' before `)'.
   dotspacemacs-smart-closing-parenthesis nil

   ;; Whitespace cleanup on save:
   dotspacemacs-whitespace-cleanup
   ;; nil ; disables cleanup (default).
   ;; 'all ; aggressively deletes empty lines and long sequences of whitespace.
   ;; 'trailing ; deletes only the whitespace at end of lines.
   'changed ; deletes whitespace only for changed lines.

   ;; Server:
   ;; Will quit functions be advised to leave the server running?
   dotspacemacs-persistent-server nil

   ;; Search Tools:
   ;; Spacemacs uses the first installed tool of the list. Supported tools are "rg", "ag", "pt", "ack" and "grep".
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")


   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   dotspacemacs-frame-title-format "%f (%I)"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; WTF does this do???
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq-default exec-path-from-shell-check-startup-files nil) ; Don't worry about where I set environment variables. You're not my mom!

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
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

;;;; Helpful Procedures

  (defmacro my-let (&rest BINDINGS.EXPRESSION)
    "Bind BINDINGS and then evaluate EXPRESSION.
Bindings of the form SYMBOL (ARGS BODY) are bound as procedures.
Bindings of the form SEQUENCE SEQUENCE are pattern-matched.
Other bindings are bound as usual."
    ;; TODO: Make this macro less ugly.
    (seq-let (e &rest rev-bs) (reverse BINDINGS.EXPRESSION) ; Reverse to get the expression at the end.
      (cl-labels
          ((let-helper
            (BINDINGS EXPRESSION)
            (if BINDINGS
                (seq-let (var val &rest bs) BINDINGS
                  (cond
                   ((and (symbolp var)
                         (consp val)
                         (listp (car val)))
                    ;; Bind procedure.
                    `(cl-labels ((,var ,(car val) ,@(cdr val)))
                       ,(let-helper bs EXPRESSION)))
                   ((sequencep var)
                    ;; Pattern-match sequence.
                    `(seq-let ,var ,val
                       ,(let-helper bs EXPRESSION)))
                   (t
                    ;; Bind as usual.
                    `(let ((,var ,val))
                       ,(let-helper bs EXPRESSION)))))
              EXPRESSION)))
        (let-helper (reverse rev-bs) e)))) ; Reverse to get the bindings in the right order again.

  (defmacro my-if (&rest CONDITIONS)
    (my-let
     (final &rest rev-c) (reverse (seq-partition CONDITIONS 2))
     cs (reverse
         `((t (error "Fallthrough on incomplete match"))
           ,(if (= 1 (length final))
                (cons t final) ; final else-clause; fallthrough error never reached.
              final) ; final then-clause.
           ,@rev-c))
     `(cond ,@cs)))

  (defun my-princ (OBJECT &optional PRINT-CHAR-FUNCTION)
    "`princ' but does not print the colon of a keyword"
    (princ (if (keywordp OBJECT)
               (substring (symbol-name OBJECT) 1)
             OBJECT)
           PRINT-CHAR-FUNCTION))

  (defun my-mkstr (&rest ARGS)
    "Create a string from arbitrary arguments."
    (with-output-to-string
      (mapc #'my-princ ARGS)))

  (defun my-isymb (&rest ARGS)
    "Create a new interned symbol."
    (intern (apply #'my-mkstr ARGS)))

  (defun my-nsymb (&rest ARGS)
    "Create a new uninterned symbol."
    (make-symbol (apply #'my-mkstr ARGS)))

;;; Hooks

  (defmacro my-make-hook (WHEN PROCEDURE &rest CONTINGENT)
    "Set up a hook to run WHEN PROCEDURE.
Create variable WHEN-PROCEDURE-hook and assign it the value CONTINGENT.
Create function WHEN-PROCEDURE-hook to run WHEN PROCEDURE-hook using `run-hooks'.
Use `advice-add' to add run-WHEN-PROCEDURE-hook as advice to PROCEDURE."
    (declare (indent 2))
    (my-let
     hook (my-isymb WHEN "-" PROCEDURE "-hook")
     `(progn
        (defvar ,hook ',CONTINGENT
          ,(my-mkstr "procedures to run " WHEN " `" PROCEDURE "'"))
        (defun ,hook (&rest _)
          ,(my-mkstr "Use `run-hooks' to run `" hook "'.")
          (run-hooks ',hook))
        (advice-add ',PROCEDURE ,WHEN #',hook
                    '((name . ,hook)
                      (depth . -100))))))

  (defun my-hook-up (HOOKS FUNCTIONS)
    "Hang all FUNCTIONS, in order, on all HOOKS."
    (seq-doseq (h HOOKS)
      (seq-doseq (f (seq-reverse FUNCTIONS))
        (add-hook h f))))

;;; Buffers and Panes

  (defmacro my-with-buffer (BUFFER &rest BODY)
    "If BUFFER is not nil, execute BODY in BUFFER. Otherwise, execute BODY in the current buffer."
    (declare (indent 1))
    `(save-current-buffer
       (and ,BUFFER (set-buffer ,BUFFER))
       ,@BODY))

;;; Track primary pane.
  (defvar my-primary-pane (frame-selected-window)
    "The pane that has an active mode-line.")

  (defun my-set-primary-pane (&rest _)
    "Set the primary pane."
    (my-let
     p (frame-selected-window)
     (unless (minibuffer-window-active-p p)
       (setq my-primary-pane p))))

  (defun my-primary-pane-active? ()
    (eq my-primary-pane (selected-window)))

  (my-make-hook :after select-frame)

  (my-make-hook :after handle-select-window)

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
      (count-lines (point-min) (point-max))))

  (defun my-set-buffer-line-count (&rest _)
    (setf my-buffer-line-count (my-buffer-line-count)))

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
    (my-let
     file (buffer-file-name BUFFER)
     (when file (abbreviate-file-name (file-truename file)))))

  (defun my-primary-file-or-buffer-name ()
    "The name of the file or buffer in the primary pane."
    (my-let
     b (window-buffer my-primary-pane)
     (or (my-buffer-file-path b)
         (buffer-name b))))

  ;;; The hook for this may be failing and messing things up.
  ;; (defvar-local my-file-vc-status nil
  ;;   "The version-control status of the current file.")
  ;; (defun my-file-vc-status (&optional FILE)
  ;;   "The version-control status of FILE or the file visited by the current buffer."
  ;;   (let ((f (or FILE (my-buffer-file-path))))
  ;;     (and f (vc-state f))))
  ;; (defun my-set-file-vc-status (&rest _)
  ;;   "Set the buffer-local variable `my-file-vc-status' to the version-control status of the file visited by the current buffer."
  ;;   (setf my-file-vc-status (my-file-vc-status)))

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
  ;;  '(my-set-file-vc-status))

  ;; (defun my-file-vc-status-string ()
  ;;   "A string that represents the VC status of the file visited by the current buffer."
  ;;   (pcase my-file-vc-status
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
    (declare (pure t) (side-effect-free t))
    (length (number-to-string N)))

;;; Strings:

  (defun my-pad (W S)
    "Integer -> String -> String
Pad string S with spaces to width W. A negative width means add the padding on the right."
    (declare (pure t) (side-effect-free t))
    (format (concat "%" (number-to-string W) "s") S))

;;; Colors

  (defun my-blend-colors (C1 C2)
    "(R G B) -> (R G B) -> (R G B)
Evenly blend C1 and C2, two emacs RGB triplets."
    (declare (pure t) (side-effect-free t))
    (seq-mapn (lambda (X Y) (* 0.5 (+ X Y)))
              C1 C2))

  (defun my-intensify-color (COLOR REFERENCE)
    "(R G B) -> (R G B) -> (R G B)
Shift COLOR away from REFERENCE."
    (declare (pure t) (side-effect-free t))
    (seq-mapn (lambda (C R)
                (* 0.5 (+ C (if (> C R) 1 0))))
              COLOR REFERENCE))

;;; Fonts & Faces

  (defun my-select-font (&rest FONTS)
    "Return the first available font in FONTS, or the default font if none are available."
    (my-if (null FONTS) (face-attribute 'default :family)
           (member (car FONTS) (font-family-list)) (car FONTS)
           (apply #'my-select-font (cdr FONTS))))

  (defun my-def-faces (GROUP &rest FACES)
    "Create FACES (name docstring properties) in GROUP. No fancy business here; the display is always t."
    (declare (indent 1))
    (cl-loop
     for (name docstring . properties) in FACES
     do (custom-declare-face name `((t . ,properties)) docstring :group GROUP)))

  (defun my-set-face-attributes (L &optional BUFFER)
    "From list L of (face :attr-1 a1 :attr-2 a2 ...) lists, give each face its attributes. Create undefined faces."
    (cl-loop for (face . attributes) in L
             do
             (unless (facep face) (make-face face))
             (apply 'set-face-attribute face BUFFER attributes)))

  (defun my--shift-face-foreground (FUNCTION FACE REFERENCE)
    "Set FACE's foreground to the result of applying FUNCTION to REFERENCE's foreground and background."
    (my-let
     color-of ((KEY)
               (color-name-to-rgb (face-attribute REFERENCE KEY nil 'default)))
     (set-face-attribute
      FACE
      nil
      :foreground (apply #'color-rgb-to-hex
                         (funcall FUNCTION
                                  (color-of :foreground)
                                  (color-of :background))))))

  (defun my-fade-face-foreground (FACE REFERENCE)
    "Make FACE's foreground a less intense version of REFERENCE's.
REFERENCE is used to avoid fading FACE into oblivion with repreated applications."
    (my--shift-face-foreground #'my-blend-colors FACE REFERENCE))

  (defun my-intensify-face-foreground (FACE REFERENCE)
    (my--shift-face-foreground #'my-intensify-color FACE REFERENCE))

  ;;; Key Maps

  (defun my-def-keys (MAP &rest BINDINGS)
    "Create new key bindings in MAP.
Each binding should be a string that can be passed to `kbd' followed by an interactive procedure."
    (declare (indent defun))
    (seq-doseq (b (seq-partition BINDINGS 2))
      (define-key MAP (kbd (seq-elt b 0))
        (seq-elt b 1))))

  ;;; ----------------------------------------------
  ;;   ;;; Mode Line, Header Line, and Frame Title Format

  (defvar adaptive-faces-setup ()
    "a list of adaptive face setup functions")

  (defun my-def-adaptive-faces (GROUP &rest ADAPTIVE-FACES)
    "Create ersatz faces in customization group GROUP.
Each ADAPTIVE-FACE take the form (NAME DOCSTRING ACTIVE-ATTRIBUTES INACTIVE-ATTRIBUTES &optional ACTIVE-SETUP INACTIVE-SETUP).

Each face should be used by calling (GROUP-NAME-face).

The active or inactive version can be modified by setting the attributes of GROUP-NAME-active-face or GROUP-NAME-inactive-face.

FACE-SETUP should a procedure of 2 arguments (faces) that sets attributes of the first argument relative to the second; the :inherit of the active faces will be used for the second."
    (my-let
     def-adaptive-face
     ((name doc active inactive &optional face-setup)
      (my-let
       face-symbol ((s) (my-isymb GROUP "-" name s "-face"))
       active-name (face-symbol "-active")
       inactive-name (face-symbol "-inactive")
       getter-name (face-symbol "")
       (progn
         (my-def-faces GROUP
           `(,active-name ,doc ,@active)
           `(,inactive-name ,doc ,@inactive))
         (fset getter-name
               `(lambda ()
                  (if (my-primary-pane-active?)
                      ',active-name
                    ',inactive-name)))
         (when face-setup
           (add-hook 'adaptive-faces-setup
                     `(lambda ()
                        (,face-setup ',active-name
                                     ',(face-attribute active-name :inherit))
                        (,face-setup ',inactive-name
                                     ',(face-attribute inactive-name :inherit))))))))
     (seq-doseq (f ADAPTIVE-FACES)
       (apply #'def-adaptive-face f))))

  (my-def-adaptive-faces
   'my-statusbar
   '(default
      "an alias af mode-line and mode-line-inactive faces"
      (:inherit mode-line)
      (:inherit mode-line-inactive))
   '(highlight
     "an emphasized face for the mode-line"
     (:weight bold
              :underline t
              :inherit my-statusbar-default-active-face)
     (:weight bold
              :underline t
              :inherit my-statusbar-default-inactive-face)
     my-intensify-face-foreground)
   '(shadow
     "a dimmed face for the mode-line"
     (:inherit my-statusbar-default-active-face)
     (:inherit my-statusbar-default-inactive-face)
     my-fade-face-foreground)
   )

  (defun my-reset-statusbar-faces ()
    (run-hooks 'adaptive-faces-setup))
  (my-reset-statusbar-faces)

  (my-make-hook :after load-theme
    my-reset-statusbar-faces)

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
    "Show whether a file-like buffer has been modified since its last save; click to save."
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
       (propertize "(" 'face (my-statusbar-shadow-face))
       (propertize
        (replace-regexp-in-string " Git[:\-]" "" vc-mode)
        'mouse-face (my-statusbar-default-face)
        'local-map (make-mode-line-mouse-map 'mouse-1 #'magit-status))
       (propertize ")" 'face (my-statusbar-shadow-face))
       )))

  (defun my-line-position ()
    "Current line / total lines. Click to toggle line numbers."
    (my-let
     lines (number-to-string my-buffer-line-count)
     (propertize
      (concat
       (my-pad (length lines) (format-mode-line "%l"))
       (propertize "/" 'face (my-statusbar-shadow-face))
       lines)
      'help-echo (if (bound-and-true-p linum-mode) "Hide line numbers." "Show line numbers.")
      'local-map (make-mode-line-mouse-map 'mouse-1 #'linum-mode))))

  ;; ;;; TODO: Create shortened mode-line faces for a collapsed but visible mode line.

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
            "Source Code Pro"
            "IBM 3720"
            "DejaVu Sans Mono"
            "Monaco"
            "Lucida Console"
            ))

  (set-face-attribute
   'variable-pitch nil
   :family (my-select-font
            "ET Book"
            "ETBembo"
            "Bembo Book MT Std"
            "Bembo MT Book Std"
            "Garamond Premier Pro"
            "Garamond Premr Pro"
            "Adobe Garamond Expert"
            "Garamond"
            ))

  (defun my-reset-font-height-by-platform ()
    "Make the font bigger if running linux, because my laptop runs linux and my desktop runs Windows."
    (my-let
     h (if (string= system-type "gnu/linux") 148 120)
     (seq-doseq (f [
                    default
                    fixed-pitch
                    variable-pitch
                    ])
       (set-face-attribute f nil :height h))))

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
  ;; ;;; Set Evil to not behave like Vim.
  (customize-set-variable 'evil-move-beyond-eol t) ; Allow the cursor to move beyond the end of the line.
  (customize-set-variable 'evil-move-cursor-back nil) ; Don't move the cursor when exiting insert mode.

  ;; ;; Flip Vi a/A behavior.
  ;; (my-def-keys evil-normal-state-map
  ;;   "a" #'evil-append-line
  ;;   "A" #'evil-append
  ;;   )

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
  ;; (my-def-keys evil-motion-state-map
  ;;  "E" #'my-evil-forward-word-end
  ;;  "e" #'my-evil-forward-WORD-end)

  ;;; ----------------------------------
  ;;; Hooks

  ;; Refresh VC state to update mode line info. Fall back to expensive vc-find-file-hook if `vc-refresh-state' is not available.

  ;; (my-make-hook :after 'magit-run-git)

  ;; (my-make-hook :after 'magit-start-process)

  ;; (my-hook-up
  ;;  '(
  ;;    after-magit-run-git-hook
  ;;    after-magit-start-process-hook
  ;;    )
  ;;  `(
  ;;    ,(if (fboundp 'vc-refresh-state) 'vc-refresh-state 'vc-find-file-hook)
  ;;     ,(lambda () (force-mode-line-update t)) ; refresh all mode lines.
  ;;     ))

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
     flyspell-mode
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

  ;; Ignore mouse-wheel left and right.
  (my-def-keys global-map
    "<mouse-6>" #'ignore
    "<mouse-7>" #'ignore
    )

  (unless (string= system-type "gnu/linux")
    (my-def-keys help-mode-map
      "<mouse-4>" #'help-go-back ; Windows mouse back (Linux mouse wheel up)
      "<mouse-5>" #'help-go-forward ; mouse forwards
      ))

  ;; Navigate wrapped lines.
  (my-def-keys evil-normal-state-map
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    )

  ;; Paste with Ctrl p.
  (my-def-keys evil-insert-state-map
    "C-p" #'evil-paste-after)

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
  (seq-doseq (x '[
                  [my-zoom-in "C-<mouse-4>" "C-<wheel-up>"]
                  [my-zoom-out "C-<mouse-5>" "C-<wheel-down>"]
                  ])
    (global-set-key
     (kbd (aref x (if (string= system-type "gnu/linux") 1 2)))
     (aref x 0)))

  ;;; Use hex for unicode character input.
  (setq-default read-quoted-char-radix 16)

  ;; ;;; Mouse & copy / paste / delete
  (setq
   ;;  ;; mouse-drag-copy-region t ; Copy on select -- disable for acme-mouse.
   ;;  delete-selection-mode t ; Allow typing over the selection. Only useful when there's a selection in insert state?
   kill-do-not-save-duplicates t ; Don't copy identical text twice.
   )

  ;;; -------------------------
  ;;; Major Mode Configurations

  ;;; Lisps
  (setq-default
   lisp-minor-modes
   '(
     aggressive-indent-mode
     paren-face-mode
     smartparens-mode
     ))

  ;;; Git
  ;; Use spacemacs for editing git commit messages.
  (global-git-commit-mode t)

  ;;;Ranger
  ;; Don't annoy me with constant messages obscuring important minibuffer information.
  ;; (my-make-hook :after 'ranger-kill-buffers-without-window
  ;;                   ((lambda () (message nil))))

  ;; ;;; Elisp:
  (my-hook-up
   '(emacs-lisp-mode-hook)
   lisp-minor-modes)

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

  ;;; Shen
  (add-to-list 'auto-mode-alist '("\\.shen$" . shen-mode))
  (my-hook-up
   '(shen-mode-hook)
   lisp-minor-modes)

 ;;; ---------------------------------------
  ;;; Lastly, some hackish theming:
  ;;; The main point is to, as much as possible without being distracting, distinguish stuff that does stuff from stuff that does not do stuff and things that look similar and act differently.

  (defun my-box->lines (FACE)
    (my-let
     color (pcase (face-attribute FACE :box)
             (`nil nil)
             (`t (face-attribute 'default :color))
             ((and (pred stringp) c) c)
             (plist (plist-get plist :color)))
     (when color (set-face-attribute
                  FACE nil :box nil :underline color :overline color))))

  (defun my-laser-minor-theme (&optional COLOR)
    "Add borders to the mode-line and disable its background color."
    (interactive)
    (my-let
     c (if COLOR COLOR
         (face-attribute 'my-statusbar-active-face :foreground nil 'default))
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
    (my-let
     unbox ((FACE)
            (unless (equal
                     (face-attribute 'default :background)
                     (face-attribute FACE :background nil 'default))
              (set-face-attribute
               FACE nil
               :box nil
               :underline nil
               :overline nil)))
     (progn
       (unbox 'mode-line)
       (unbox 'mode-line-inactive))))

  (defun my-theme-tweaks ()
    "Tweak faces to simplify themes."
    (my-set-face-attributes
     '(
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
