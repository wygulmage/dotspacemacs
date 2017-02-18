;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup. It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
This function should only set values."
  (setq-default
   ;; Base setup, a layer contained in the directory `+distribution':
   dotspacemacs-distribution
   ;; 'spacemacs ; default
   'spacemacs-base ; minimal

   ;; Package downloading & retention:
   dotspacemacs-download-packages
   ;; 'used ; (default) only downloads explicitly used packages and removes any unused packages and dependencies.
   'used-but-keep-unused ; only downloads used packages but won't delete unused packages.
   ;; 'all ; downloads all the packages regardless of whether they are used, and won't delete packages.

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
   dotspacemacs-configuration-layer-path '() ; default '()

   ;; Configuration layers to install & load:
   dotspacemacs-configuration-layers
   '(
     (colors :packages
             rainbow-mode ; for color strings only
             :variables
             rainbow-x-colors nil
             rainbow-html-colors nil)
     (org :variables
          org-enable-github-support t)
     (ranger :variables
             ranger-override-dired t
             ranger-show-preview t
             ranger-show-literal nil)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spacemacs-completion
     spacemacs-editing
     (spacemacs-evil :packages
                     (not vi-tilde-fringe))
     spacemacs-ui ; includes restart-emacs
     ;;; Bindings:
     better-defaults
     vinegar ; dired
     ;;; Checking & Completion:
     auto-completion
     ivy
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     flyspell-sort-corrections nil)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil)
     ;;; Languages:
     (semantic :packages
               semantic
               srefactor)
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
     ;; (acme-mouse :location (recipe :fetcher github :repo "akrito/acme-mouse")) ; does not work in Spacemacs.
     adaptive-wrap
     aggressive-indent
     company
     dash ; list functions
     paren-face
     (shen-elisp
      :location (recipe :repo "deech/shen-elisp"
                        :fetcher github
                        :files ("shen*.el"))
      :upgrade 't)
     )

   ;; Packages that will not be updated:
   dotspacemacs-frozen-packages '() ; default '()

   ;; Packages and extensions that will not be installed or loaded:
   dotspacemacs-excluded-packages
   '(
     fancy-battery ; The GUI shell shows this.
     helm ; Use ivy instead.
     highlight-indentation ; Indentation shows this.
     highlight-parentheses ; Use paren-face-mode instead.
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
   ;; Will ELPA repositories be contacted via HTTPS? Disable only if you have no way to use HTTPS. Launching Emacs with the parameter --insecure sets this variable to nil.
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
   ;; 'hybrid ; like vim except that insert state is replaced by the hybrid state with emacs key bindings. The value can also be a list with :variables keyword. Check the editing styles section of the documentation for details on available variables.

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
   dotspacemacs-startup-lists '((recents . 7)
                                (projects . 7))
   ;; Will the startup buffer resize?
   dotspacemacs-startup-buffer-responsive t ; default t

   ;; Default major mode of the scratch buffer:
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

   ;; Default font or prioritized list of fonts:
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
   ;; Will `p' cycle through the kill ring content?
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
   dotspacemacs-switch-to-buffer-prefers-purpose t; default nil

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
   ;; '(:relative nil ; A property list can be used for finer control.
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)


   ;; Code folding:
   dotspacemacs-folding-method
   'evil ; (default)
   ;; 'origami

   ;; Scope for highlighting delimiters:
   ;; Possible values are `any', `current', `all' or `nil'. `all' highlights any scope and emphasizes the current one.
   dotspacemacs-highlight-delimiters 'current ; default nil

   ;;; Smartparens
   ;; Will `smartparens-strict-mode' be enabled in `prog-mode'?
   dotspacemacs-smartparens-strict-mode nil ; default nil
   ;; Will `)' in insert mode pass over any automatically added closing parenthesis, bracket, quote, etc.? This can be temporary disabled by pressing `C-q' before `)'.
   dotspacemacs-smart-closing-parenthesis nil ; default nil

   ;; Whitespace cleanup on save:
   dotspacemacs-whitespace-cleanup
   ;; nil ; (default) disables cleanup.
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

  (setq the-default-mode-line mode-line-format) ; Save in case you want to know.

  (customize-set-variable 'adaptive-fill-regexp "[ \t]*\\([-–!|#%;>·•‣⁃◦]+[ \t]*\\)*") ; Removed '*' so I can make non-unicode bullet lists. Ideally there should be two separate variables: adaptive-fill-regexp and adaptive-indent-regexp. The first would indent with the 'whitespace' character, but the second would indent with actual whitespace.

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization, after layers configuration. Put your configuration code--except for variables that should be set before a package is loaded--here."

;;; ------------------
;;; Helpful Procedures

  ;; TODO: Implement compose function.

  (defun my-customize-set-variables (&rest assocs)
    "Takes zero or more ('symbol . value) arguments and customizes symbol to value."
    (dolist (key.val assocs)
      (customize-set-variable (car key.val) (cdr key.val))))

;;; Hooks:

  (defun my-hook-up (mode-hooks hook-functions)
    "Add all hook-functions to all mode-hooks."
    (dolist (mode-hook mode-hooks)
      (dolist (hook-function hook-functions)
        (add-hook mode-hook hook-function))))

  (defun my-add-procedure-hook (hook when procedure)
    "Run hook :before or :after procedure.
Warning: will create a null hook if the hook is not defined."
    (unless (boundp hook) (defvar hook nil))
    (pcase when
      (:before
       (add-function :before procedure
                     (lambda (ignored) (run-hooks hook))))
      (:after
       (add-function :filter-return procedure
                     (lambda (result) (run-hooks hook) result)))))

  (defun my-add-hooks-to-procedures (hooks when procedures)
    "Run all hooks :before or :after all procedures.
Warning: will create null hooks if hooks are not defined."
    (dolist (hook hooks)
      (dolist (procedure procedures)
        (my-add-procedure-hook hook when procedure))))

;;; Buffers and panes:

  (defvar my-current-pane (frame-selected-window)
    "The pane that has an active mode-line")

  (defun my-set-current-pane ()
    "Sets my-current-pane to the active pane (unless the active pane is a minibuffer)."
    (let ((p (frame-selected-window)))
      (unless (minibuffer-window-active-p p)
        (setq my-current-pane p))))

  (defun my-unset-current-pane ()
    "Sets my-current-pane to nil."
    (setq my-current-pane nil)
    (force-mode-line-update))

  (add-hook 'focus-out-hook 'my-unset-current-pane)

  (my-add-hooks-to-procedures
   '(my-after-switch-frame-hook)
   :after
   '(
     handle-switch-frame
     select-window
     ))

  (my-hook-up
   '(
     focus-in-hook
     window-configuration-change-hook
     )
   '(my-set-current-pane))

  (defun my-current-pane-active? ()
    (eq my-current-pane (selected-window)))

  (defun my-buffer-line-count ()
    "Number of lines in the current buffer. If the last line of the buffer is empty, it won't be counted."
    (count-lines (buffer-end -1) (buffer-end 1)))

;;; Numbers:

  (defun my-digits (n)
    "Number -> String
The number of decimal digits in n, including any period as a digit."
    (length (number-to-string n)))

;;; Strings, Colors, and Faces:

  (defun my-define-faces (group &rest faces)
    "Creates faces (name docstring properties) in group. No fancy business here; the display is always t. Currently broken."
    (dolist (face faces)
      (let ((name (car face))
            (docstring (cadr face))
            (properties (cddr face)))
        (defface name (list (cons t properties)) docstring :group group))))

  (defun my-pad (w s)
    "Integer -> String -> String
Pad string s to width w; a negative width means add the padding on the right."
    (format (concat "%" (number-to-string w) "s") s))

  (defun my-shift-color (color reference &optional away?)
    "Shift color toward or away from reference. Color and reference should be emacs color triples."
    (let ((toner (cond ((not away?) reference)
                       ((> (apply '+ color)
                           (apply '+ reference))
                        (color-values "white"))
                       (t '(0 0 0)))))
      (my-blend-colors color toner)))

  (defun my-shift-face-color (face reference &optional fade?)
    "Make a face's foreground a more or less intense version of another face's."
    (cl-flet ((color-of (key)
                        (color-values (face-attribute reference key))))
      (let ((color (my-color-values-to-string
                    (my-shift-color (color-of :foreground)
                                    (color-of :background)
                                    (not fade?)))))
        (set-face-attribute face nil :foreground color))))

  (defun my-select-font (fonts)
    "Return the first available font in `fonts', or the default font if none are available."
    (cond ((null fonts) (face-attribute 'default :family))
          ((member (car fonts) (font-family-list)) (car fonts))
          (t (my-select-font (cdr fonts)))))

  (defun my-set-face-attributes (l &optional buffer)
    "From a list of (face :attr-1 a1 :attr-2 a2 ...) lists, give each face its attributes. Create undefined faces."
    (dolist (x l)
      (let ((face (car x))
            (attributes (cdr x)))
        (unless (facep face) (make-face face))
        (apply 'set-face-attribute face buffer attributes))))

  (defun max-color-val ()
    "The current maximum value for emacs color triplets."
    (car (color-values "white")))

  (defun my-color-values-to-string (c)
    "Create a color string from and Emacs numerical color triplet."
    (let ((color-ratio (/ (max-color-val) 255)))
      (cl-multiple-value-bind (r g b)
          (mapcar (lambda (x) (truncate x color-ratio)) c)
        (format "#%02X%02X%02X" r g b))))

  (defun my-blend-colors (c1 c2)
    "Evenly blend two emacs color triplets."
    (-zip-with (lambda (x y)
                 (truncate (+ x y) 2))
               c1 c2))

  ;;; ----------------------------------------------
  ;;; Mode Line, Header Line, and Frame Title Format

  (defface my-active-statusbar-face
    '((t :inherit mode-line))
    "an alias for mode-line face.")
  (defface my-inactive-statusbar-face
    '((t :inherit mode-line-inactive))
    "an alias for mode-line-inactive face.")
  (defface my-active-statusbar-shadow-face
    '((t :inherit my-active-statusbar-face))
    "an alias for mode-line face.")
  (defface my-inactive-statusbar-shadow-face
    '((t :inherit my-inactive-statusbar-face))
    "an alias for mode-line-inactive face.")

  (defun my-get-statusbar-face ()
    "an ersatz face that switches between active- and inactive-statusbar-face"
    (if (my-current-pane-active?)
        'my-active-statusbar-face
      'my-inactive-statusbar-face))

  (defun my-get-statusbar-shadow-face ()
    "an ersatz face that switches between active- and inactive-statusbar-shadow-face"
    (if (my-current-pane-active?)
        'my-active-statusbar-shadow-face
      'my-inactive-statusbar-shadow-face))

  (defun my-reset-statusbar-faces ()
    "Sets statusbar shadow faces to be faded versions of their counterparts."
    (cl-flet
        ((fade (faded reference)
               (set-face-attribute
                faded
                nil
                :foreground (my-color-values-to-string
                             (my-shift-color
                              (color-values (face-attribute reference :foreground nil t))
                              (color-values (face-attribute reference :background nil t)))))))
      (fade 'my-active-statusbar-shadow-face 'my-active-statusbar-face)
      (fade 'my-inactive-statusbar-shadow-face 'my-inactive-statusbar-face)))
  (my-reset-statusbar-faces)
  (add-hook 'after-load-theme-hook 'my-reset-statusbar-faces)

  ;; To do: Check for derived mode to determine whether buffer is file-like. prog-mode and text-mode will hopefully do it. Do the same for mode line?

  (defun my-buffer-name ()
    "The name of the buffer. If it's a file, shows the directory on hover and opens dired with a click."
    (if buffer-file-truename
        (propertize (buffer-name)
                    'help-echo (abbreviate-file-name buffer-file-truename)
                    'local-map (make-mode-line-mouse-map
                                'mouse-1 (lambda () (interactive)
                                           (dired (file-name-directory buffer-file-truename)))))
      (buffer-name)))

  (defun my-buffer-or-file-name ()
    "The filename if there is one; otherwise, the buffer name"
    (if buffer-file-truename
        (file-name-nondirectory buffer-file-truename)
      (buffer-name)))

  (defun my-file-directory ()
    "The directory of the current file."
    (if buffer-file-truename
        (file-name-directory (abbreviate-file-name buffer-file-truename))
      ""))

  (defun my-major-mode-name ()
    "The buffer's major-mode"
    (propertize mode-name
                'help-echo "Click mouse 1 for mode menu, mouse 2 for mode info, or mouse 3 to toggle minor modes."
                'local-map mode-line-major-mode-keymap))

  (defun my-buffer-write-status ()
    "Show whether a file-like buffer has been modified since its last save; click to save. Should 'do what I mean'."
    (if (not (or buffer-file-name
                 (derived-mode-p 'text-mode 'prog-mode)))
        "" ; Ignore buffers that aren't files.
      (propertize
       (my-pad 1 (concat (if (buffer-modified-p) "◆" "")
                         (if buffer-read-only "🔒" "")))
       'help-echo
       (concat (if (buffer-modified-p) "modified " "")
               (if buffer-read-only "read-only " "")
               (if buffer-file-name "file " "buffer ")
               "‑ click to save")
       'local-map (make-mode-line-mouse-map 'mouse-1 #'save-buffer))))

  (defun my-simpler-vc-branch ()
    (if (not vc-mode)
        ""
      (concat
       (propertize "(" 'face (my-get-statusbar-shadow-face))
       (propertize
        (replace-regexp-in-string " Git[:\-]" "" vc-mode)
        'local-map (make-mode-line-mouse-map 'mouse-1 #'magit-status))
       (propertize ")" 'face (my-get-statusbar-shadow-face))
       )))

  (defun my-line-position ()
    "Current line / total lines. Click to toggle line numbers."
    (let ((lines (number-to-string (my-buffer-line-count))))
      (propertize
       (concat (my-pad (length lines)
                       (format-mode-line "%l"))
               (propertize "/" 'face (my-get-statusbar-shadow-face))
               lines)
       'help-echo "Toggle line numbers."
       'local-map (make-mode-line-mouse-map 'mouse-1 #'linum-mode))))

  (defvar my-base-mode-line-format
    '(
      (:eval (my-buffer-write-status))
      " "
      (:eval (my-buffer-name))
      " "
      (:eval (my-simpler-vc-branch))
      "  "
      (:eval (my-line-position))
      )
    "a simple status bar")

  (defun my-format-text-mode-line ()
    (setq mode-line-format my-base-mode-line-format))

  (defvar my-prog-mode-line-format
    (append
     my-base-mode-line-format
     '(
       "  "
       (:eval (my-major-mode-name))
       "  "
       (:eval (when (bound-and-true-p anzu-mode)(anzu--update-mode-line)))
       ))
    "simple status bar that indicates the current mode")

  (defun my-format-prog-mode-line ()
    (setq mode-line-format my-prog-mode-line-format))

  (defun my-format-frame-title ()
    (when (display-graphic-p)
      (setq frame-title-format
            '(
              (:eval (my-buffer-write-status))
              " "
              (:eval (my-file-directory))
              (:eval (my-buffer-or-file-name))
              ))))
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

  ;; (defun my-reset-font-height-by-platform ()
  ;;   (let ((h (if (string= system-type "gnu/linux") 148 120)))
  ;;     (mapc (lambda (face) (set-face-attribute face nil :height h))
  ;;           '(default fixed-pitch variable-pitch ))))

  (defun my-reset-font-height-by-platform ()
    (let ((h (if (string= system-type "gnu/linux") 148 120)))
      (dolist (face '(
                      default
                      fixed-pitch
                      variable-pitch
                      ))
        (set-face-attribute face nil :height h))))

  (add-hook 'window-setup-hook 'my-reset-font-height-by-platform)

  ;;; ---------------------------------
  ;;; Miscelaneous Global Stuff

  (global-hl-line-mode -1) ; Disable current line highlight.
  (global-visual-line-mode 1) ; Always wrap lines to window.
  (setq-default major-mode 'text-mode) ; Use text instead of fundamental.
  (setq vc-follow-symlinks t)

  ;;; ----------------------------------
  ;;; Set Evil to not behave like Vim.
  (customize-set-variable 'evil-move-beyond-eol t) ; Allow the cursor to move beyond the end of the line.
  (customize-set-variable 'evil-move-cursor-back nil) ; Don't move the cursor when exiting insert mode.

  ;; ;; Make the cursor always a bar.
  ;; (dolist (cursor '(
  ;;                   evil-emacs-state-cursor
  ;;                   evil-insert-state-cursor
  ;;                   evil-motion-state-cursor
  ;;                   evil-normal-state-cursor
  ;;                   evil-visual-state-cursor))
  ;;   (set cursor 'bar))

  ;; Flip Vi a/A behavior.
  (define-key evil-normal-state-map "a" 'evil-append-line)
  (define-key evil-normal-state-map "A" 'evil-append)

  (defun my-evil-forward-end (thing &optional count)
    "Move forward past the end of thing. Repeat count times."
    ;; (unless (eobp) (forward-char))
    (forward-thing thing (or count 1)))

  (evil-define-motion my-evil-forward-word-end (count &optional bigword)
    "Move the cursor past the end of the count-th next word."
    :type inclusive ; I don't know what this does!
    (let ((thing (if bigword
                     'evil-WORD
                   'evil-word))
          (n (or count 1)))
      (evil-signal-at-bob-or-eob n)
      (my-evil-forward-end thing n)))

  (evil-define-motion my-evil-forward-WORD-end (count)
    "Move the cursor past the end of the count-th next WORD."
    :type exclusive ; I don't know what this does!
    (my-evil-forward-word-end count t))

  ;; Flip Vi e/E behavior to make a more useful distiction from w/W.
  (define-key evil-motion-state-map "E" 'my-evil-forward-word-end)
  (define-key evil-motion-state-map "e" 'my-evil-forward-WORD-end)


  ;;; ----------------------------------
  ;;; Hooks

  ;; * first-change-hook is called immediately before changing an unmodified buffer.
  ;; * after-change-major-mode-hook
  ;; * buffer-list-update-hook
  ;; * magit-refresh-buffer-hook


  ;; (add-hook 'minibuffer-inactive-mode-hook (lambda () (setq max-mini-window-height 0))) ; -- does not work; you can't make the minibuffer zero lines.

  (my-hook-up
   '(
     after-change-major-mode-hook
     after-save-hook
     buffer-list-update-hook
     first-change-hook
     )
   '(force-mode-line-update))

  ;; Refresh VC state to update mode line info. Fall back to expensive vc-find-file-hook if `vc-refresh-state' is not available.

  (my-add-hooks-to-procedures
   '(my-after-magit-runs-git-hook)
   :after
   '(
     magit-run-git
     magit-start-process
     ))

  (my-hook-up
   '(my-after-magit-runs-git-hook)
   `(
     ,(if (fboundp 'vc-refresh-state)
          'vc-refresh-state 'vc-find-file-hook)
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

  ;;; Navigate wrapped lines.
  (define-key evil-normal-state-map
    (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map
    (kbd "k") 'evil-previous-visual-line)

  (evil-define-command my-greedy-delete-backward ()
    (evil-delete (save-excursion
                   (evil-backward-word-begin)
                   (point))
                 (point)
                 'exclusive
                 nil)
    (delete-horizontal-space t))

  (define-key evil-insert-state-map
    (kbd "<backspace>") 'my-greedy-delete-backward) ; Make backspace delete the whole word.

  ;;; Paste with Ctrl p.
  (define-key evil-insert-state-map
    (kbd "C-p") 'evil-paste-after)

  ;;; Zoom with Ctrl + mouse wheel.
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

  ;;; Insert unicode character with Ctrl Shift u.
  (global-set-key (kbd "C-S-u") 'insert-char)

  ;;; Use hex for unicode character input.
  (setq-default read-quoted-char-radix 16)

  ;;; Mouse & copy / paste / delete
  (setq
   mouse-drag-copy-region t ; Copy on select -- disable for acme-mouse.
   kill-do-not-save-duplicates t ; Don't copy identical text twice.
   )

  ;;; ---------------------------
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
  ;;   (add-to-list company-backends '(company-elm))
  ;;   (elm-oracle-setup-completion))
  ;; (add-hook 'elm-mode-hook 'my-elm-mode-hook)


  ;;; ---------------------------------------
  ;;; Lastly, some hackish theming:
  ;;; The main point is to, as much as possible without being distracting, distinguish stuff that does stuff from stuff that does not do stuff and things that look similar and act differently.

  ;; How to use advice to create a hook:
  (defvar after-load-theme-hook nil
    "Functions to run after a theme is loaded.")
  (my-add-procedure-hook 'after-load-theme-hook :after 'load-theme)

  (defun my-box-to-lines (face)
    (let ((color
           (pcase (face-attribute face :box)
             (`nil nil)
             (`t (face-attribute 'default :color))
             ((and (pred stringp) c) c)
             (plist (plist-get plist :color)))))
      (when color (set-face-attribute
                   face nil :box nil :underline color :overline color))))

  (defun my-laser-minor-theme (&optional color)
    "Add borders to the mode-line and disable its background color."
    (interactive)
    (let ((c
           (if color color
             (face-attribute 'shadow :foreground))))
      (my-set-face-attributes
       `(
         (mode-line :box nil
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
    (unless (equal (face-attribute 'default :background)
                   (face-attribute 'mode-line :background))
      (set-face-attribute 'mode-line nil
                          :box nil
                          :underline nil
                          :overline nil
                          :inherit font-lock-comment-face)))

  (defun my-set-shadow-face ()
    (my-shift-face-color 'shadow 'default t))

  (defun my-theme-tweaks ()
    (my-box-to-lines 'mode-line)
    (my-box-to-lines 'mode-line-inactive)
    (my-set-shadow-face)
    (my-set-face-attributes
     `(
       ;; (cursor :background) -- this is just a stub to remind me of the cursor face.
       ;;; Things that don't do stuff:
       (font-lock-comment-face
        :background unspecified
        :slant normal)
       ;; (font-lock-comment-delimiter-face :slant normal :inherit font-lock-comment-face)
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
       ;; (mode-line :inherit font-lock-comment-face)
       ;;; Things that do stuff:
       ;; (font-lock-builtin-face :inherit default)
       ;; (font-lock-constant-face :inherit default)
       (font-lock-keyword-face
        :foreground unspecified
        :inherit default)
       ;; (font-lock-type-face :inherit default)
       (font-lock-function-name-face
        :foreground unspecified
        :inherit default)
       (font-lock-variable-name-face
        :foreground unspecified
        :inherit default)
       ;;; Things that look like other things:
       (font-lock-string-face :slant italic)
       )))
  (my-theme-tweaks)
  (add-hook 'after-load-theme-hook 'my-theme-tweaks)
  )

;; Do not write anything past this comment. This is where Emacs will auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adaptive-fill-regexp "[ 	]*\\([-–!|#%;>·•‣⁃◦]+[ 	]*\\)*")
 '(package-selected-packages
   (quote
    (org-plus-contrib yapfify xterm-color ws-butler winum which-key wgrep web-mode web-beautify vimrc-mode uuidgen use-package unfill tagedit srefactor smex smeargle slim-mode shen-elisp shell-pop scss-mode sass-mode restart-emacs ranger rainbow-mode pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements pcre2el paren-face paradox ox-gfm org-projectile org-pomodoro org-download open-junk-file mwim multi-term move-text mmm-mode markdown-toc magithub magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc ivy-hydra intero info+ hy-mode hungry-delete htmlize hlint-refactor hindent help-fns+ helm-make haskell-snippets gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy flyspell-correct-ivy flycheck-pos-tip flycheck-haskell flycheck-elm flx-ido expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elm-mode elisp-slime-nav diff-hl dactyl-mode cython-mode counsel-projectile company-web company-tern company-statistics company-ghci company-ghc company-cabal company-anaconda coffee-mode cmm-mode clean-aindent-mode auto-yasnippet auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adaptive-fill-regexp "[ 	]*\\([-–!|#%;>·•‣⁃◦]+[ 	]*\\)*")
 '(package-selected-packages
   (quote
    (ox-gfm org-projectile org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot zonokai-theme zenburn-theme zen-and-art-theme yapfify yaml-mode xterm-color ws-butler window-numbering which-key wgrep web-mode web-beautify vimrc-mode uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme srefactor spacemacs-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slim-mode shen-elisp shell-pop seti-theme scss-mode sass-mode reverse-theme restart-emacs ranger rainbow-mode railscasts-theme quelpa pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pastels-on-dark-theme paren-face paradox orgit organic-green-theme open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow magit-gh-pulls macrostep lush-theme lorem-ipsum livid-mode live-py-mode linum-relative link-hint light-soap-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme ivy-hydra ir-black-theme intero inkpot-theme info+ ido-vertical-mode hy-mode hungry-delete hlint-refactor hindent heroku-theme hemisu-theme help-fns+ helm-make hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md gandalf-theme flyspell-correct-ivy flycheck-pos-tip flycheck-haskell flycheck-elm flx-ido flatui-theme flatland-theme firebelly-theme farmhouse-theme expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elm-mode elisp-slime-nav dracula-theme django-theme diff-hl darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme dactyl-mode cython-mode cyberpunk-theme counsel-projectile company-web company-tern company-statistics company-ghci company-ghc company-cabal company-anaconda color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmm-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
