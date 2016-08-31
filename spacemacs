;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration:
This function should include only set values."
  (setq-default
   ;;; Base setup, a layer contained in the directory `+distribution':
   ;; Available distributions are `spacemacs-base' and `spacemacs'.
   dotspacemacs-distribution 'spacemacs-base ; default 'spacemacs

   ;;; Package downloading and retention:
   ;; Possible values are `used', `used-but-keep-unused' and `all'.
   ;; `used' will download only explicitly used packages and remove any unused packages as well as their dependencies.
   ;; `used-but-keep-unused' will download only used packages but won't delete them if they become unused.
   ;; `all' will download all the packages regardless of whether they are used or not, and packages won't be deleted by Spacemacs.
   dotspacemacs-download-packages 'used ; default 'used

   ;;; Deferred layer installation:
   ;; Delay layer installation until opening a file with a supported type. Layers will be added to `dotspacemacs-configuration-layers' when they are installed.
   ;; `unused' will wait to install layers not listed in `dotspacemacs-configuration-layers'.
   ;; `all' will wait to install any layer that supports lazy installation, even those listed in `dotspacemacs-configuration-layers'.
   ;; `nil' disables lazy installation.
   dotspacemacs-enable-lazy-installation 'unused ; default 'unused

   dotspacemacs-ask-for-lazy-installation t ; default t; if non-nil, Spacemacs will ask for confirmation before installing a layer lazily.

   ;;; Additional paths for configuration layers:
   ;; Paths must have a trailing slash (e.g. `~/.mycontribs/').
   dotspacemacs-configuration-layer-path '()

   ;;; Configuration layers to load:
   dotspacemacs-configuration-layers
   '(
     (colors :variables ; for color strings only
             rainbow-x-colors nil
             rainbow-html-colors nil)
;;;  org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spacemacs-editing
     themes-megapack
     ;;; Bindings:
     better-defaults
     vinegar ; dired
     ivy
     ;;; Checking & Completion:
     auto-completion
     helm
     spell-checking
     syntax-checking
     ;;; Languages:
     elm
     emacs-lisp
     haskell
     javascript
     markdown
     vimscript
     ;;; VC:
     git
     github
     version-control
     )

   ;;; Packages that will be installed without being wrapped in a layer:
   ;; If you need configuration for these packages, then consider creating a layer. You can also put the configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     adaptive-wrap
     aggressive-indent
     paren-face
     popwin ; so helm [space] b b works
     )

   ;;; Packages that will not be updated:
   dotspacemacs-frozen-packages '()

   ;;; Packages and/or extensions that will not be installed or loaded:
   dotspacemacs-excluded-packages '(vi-tilde-fringe)
   ))

(defun dotspacemacs/init ()
  "Initialization function:
This function is called at the very startup of Spacemacs initialization before layers configuration. You should not put any user code in here besides modifying the variable values."
  ;;; Spacemacs settings:
  (setq-default
   ;;; ELPA:
   dotspacemacs-elpa-https t ; default t; if non-nil, ELPA repositories are contacted via HTTPS whenever possible. Set to nil only if you have no way to use HTTPS. Launching Emacs with the parameter `--insecure' sets this variable to nil.
   dotspacemacs-elpa-timeout 5 ; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-subdirectory nil ; default nil; if non-nil, a form that evaluates to a package directory. For example, to use different package directories for different Emacs versions, set this to `emacs-version'.

   ;;; The default package repository if no repository has been specified with an installed package:
   ;; Not used for now.
   dotspacemacs-default-package-repository nil ; default nil

   dotspacemacs-check-for-update t ; default t; if non-nil, spacemacs will check for updates at startup when the current branch is not `develop'.

   ;;; Editing style:
   ;; One of `vim', `emacs' or `hybrid'. `hybrid' is like `vim' except that `insert state' is replaced by the `hybrid state' with `emacs' key bindings. The value can also be a list with `:variables' keyword (similar to layers). Check the editing styles section of the documentation for details on available variables.
   dotspacemacs-editing-style 'vim ; default 'vim

   ;; If non-nil, output loading progress to `*Messages*' buffer.
   dotspacemacs-verbose-loading nil ; default nil

   ;; If non-nil, a progress bar is displayed when spacemacs is loading. This may increase the boot time; set it to nil to boost the loading time.
   dotspacemacs-loading-progress-bar nil ; default t

   ;;; The startup banner:
   ;; `official' displays the official spacemacs logo.
   ;; `random' chooses a random text banner in `core/banners' directory.
   ;; An integer value is the index of text banner.
   ;; A string value must be a path to an image format supported by your Emacs build.
   ;; If nil then no banner is displayed.
   dotspacemacs-startup-banner nil ; default 'official

   ;;; Items to show in startup buffer:
   ;; A list or an association list of of the form `(list-type . list-size)`. If nil it is disabled.
   ;; Possible values for list-type are: `recents' `bookmarks' `projects' `agenda' `todos'.
   dotspacemacs-startup-lists '((recents . 7)
                                (projects . 7))

   ;;; Default major mode of the scratch buffer:
   dotspacemacs-scratch-mode 'text-mode ; default 'text-mode

   ;;; Themes:
   ;; The first of the list is loaded when spacemacs starts. Press <SPC> T n to cycle to the next theme in the list (works great with 2 themes variants, one dark and one light).
   dotspacemacs-themes
   '(
     spacemacs-dark
     sanityinc-tomorrow-eighties
     solarized-light
     solarized-dark
     leuven
     monokai
     zenburn
     )

   ;; If non-nil, the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t

   ;;; Default font or prioritized list of fonts:
   ;; `powerline-scale' allows to quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font
   '("Source Code Pro"
     :size 13.0
     :weight normal
     :width normal
     :powerline-scale 1.1)

   ;;; The leader key:
   dotspacemacs-leader-key "SPC"
   ;;; The leader key accessible in `emacs state' and `insert state':
   dotspacemacs-emacs-leader-key "M-m" ; default "M-m"
   ;;; Major mode leader key:
   ;; A shortcut key which is the equivalent of pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key "," ; default ","
   ;;; Major mode leader key accessible in `emacs state' and `insert state':
   dotspacemacs-major-mode-emacs-leader-key "C-M-m" ; default "C-M-m"
   ;;; The key used for Emacs commands (M-x) after pressing on the leader key:
   dotspacemacs-emacs-command-key "SPC" ; default "SPC"
   ;;; The command key used for Evil commands (ex-commands) and Emacs commands (M-x):
   dotspacemacs-command-key ":" ; By default the command key is `:' so ex-commands are executed like in Vim with `:' and Emacs commands are executed with `<leader> :'.

   ;;; Vim keybindings:
   dotspacemacs-remap-Y-to-y$ t ; default t; if non-nil, `Y' is remapped to `y$'.
   dotspacemacs-retain-visual-state-on-shift t ; default t; if non-nil, the shift mappings `<' and `>' retain visual state if used there.
   dotspacemacs-visual-line-move-text nil ; default nil; if non-nil, J and K move lines up and down when in visual mode.
   dotspacemacs-ex-substitute-global nil ; default nil; if non nil, invert the meaning of `g' in `:substitute' Evil ex-command.

   ;;; Variables to control whether separate commands are bound in the GUI to the key pairs C-i, TAB and C-m, RET:
   ;; Setting it to a non-nil value allows for separate commands under <C-i> and TAB or <C-m> and RET. In the terminal, these pairs are generally indistinguishable, so this only works in the GUI.
   ;; (default nil)
   dotspacemacs-distinguish-gui-tab nil
;;; dotspacemacs-distinguish-gui-ret nil ; (not implemented)

   ;;; Layouts:
   dotspacemacs-default-layout-name "Default" ; default "Default"; Name of the default layout.
   dotspacemacs-display-default-layout nil ; default nil; if non-nil, the default layout name is displayed in the mode-line.
   dotspacemacs-auto-resume-layouts nil ; default nil; if non-nil then the last auto saved layouts are resume automatically on start.

   ;;: Size (in MB) above which spacemacs will prompt to open a large file without modes to avoid performance issues:
   dotspacemacs-large-file-size 1 ; default 1

   ;;; Where to auto-save files:
   ;; `original' auto-saves the file in-place.
   ;; `cache' auto-saves the file to another file stored in the cache directory.
   ;; `nil' disables auto-saving.
   dotspacemacs-auto-save-file-location 'cache ; default 'cache

   ;;; Maximum number of rollback slots to keep in the cache:
   dotspacemacs-max-rollback-slots 5 ; default 5

   ;;; Helm:
   dotspacemacs-helm-resize nil ; default nil; if non-nil, `helm' will try to minimize the space it uses.
   dotspacemacs-helm-no-header t ; default nil; if non-nil, the helm header is hidden when there is only one source.
   ;;; The position of `helm':
   ;; Options are `bottom', `top', `left', or `right'.
   dotspacemacs-helm-position 'bottom ; default 'bottom
   ;;; Fuzzy matching in helm:
   ;; If set to `always', force fuzzy matching in all non-asynchronous sources. If set to `source', preserve individual source settings. Else, disable fuzzy matching in all sources.
   dotspacemacs-helm-use-fuzzy 'always ; default 'always

   ;;; Paste micro-state:
   dotspacemacs-enable-paste-transient-state t ; default nil; when non-nil, pressing `p` several times cycle between the kill ring content.

   ;;; Which-key delay in seconds:
   ;; The which-key buffer is a popup listing the commands bound to the current keystroke sequence.
   dotspacemacs-which-key-delay 0.4 ; default 0.4

   ;;; Which-key frame position:
   ;; Possible values are `right', `bottom' and `right-then-bottom'. `right-then-bottom' tries to display the frame to the right; if there is insufficient space it displays it at the bottom.
   dotspacemacs-which-key-position 'bottom ; default 'bottom

   ;;; Fullscreen:
   dotspacemacs-fullscreen-at-startup nil ; default nil; if non-nil, the frame is fullscreen when Emacs starts up. (Emacs 24.4+ only)
   dotspacemacs-fullscreen-use-non-native nil ; default nil; if non-nil, `spacemacs/toggle-fullscreen' will not use native fullscreen. Use to disable fullscreen animations in OSX.
   dotspacemacs-maximized-at-startup nil ; default nil; if non-nil, the frame is maximized when Emacs starts up. Ignored if `dotspacemacs-fullscreen-at-startup' is not nil. (Emacs 24.4+ only)

   ;;; Frame opacity:
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90 ; default 90; for active and selected frames.
   dotspacemacs-inactive-transparency 90 ; default 90; for inactive and unselected frames.

   ;;; Transient states:
   dotspacemacs-show-transient-state-title t ; default t; if non-nil, show the titles of transient states.
   dotspacemacs-show-transient-state-color-guide t ; default t; if non-nil, show the color guide hint for transient state keys.

   ;; If non-nil, unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t ; default t

   ;;; Smooth scrolling:
   ;; Smooth scrolling overrides the default behavior of Emacs, which recenters the point when it reaches the top or bottom of the screen. t enables, nil disables.
   dotspacemacs-smooth-scrolling t ; default t

   ;;; Line numbers:
   dotspacemacs-line-numbers nil ; default nil; if non-nil, line numbers are turned on in all `prog-mode' and `text-mode' derivatives. If set to `relative', also turns on relative line numbers.

   ;;; Code folding:
   ;; Possible values are `evil' and `origami'.
   dotspacemacs-folding-method 'evil ; default 'evil

   ;;; Scope for highlighting delimiters:
   ;; Possible values are `any', `current', `all' or `nil'. `all' highlights any scope and emphasizes the current one.
   dotspacemacs-highlight-delimiters 'current ; default nil

   ;;; Smartparens:
   dotspacemacs-smartparens-strict-mode nil ; default nil; if non-nil, smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smart-closing-parenthesis nil ; default nil; if non-nil, pressing `)' in insert mode passes over any automatically added closing parenthesis, bracket, quote, etc… This can be temporary disabled by pressing `C-q' before `)'.

   ;;; Whitespace cleanup on save:
   ;; `all' aggressively deletes empty lines and long sequences of whitespace.
   ;; `trailing' deletes only the whitespace at end of lines.
   ;; `changed' deletes only whitespace for changed lines.
   ;; `nil' disables cleanup.
   dotspacemacs-whitespace-cleanup 'trailing ; default nil

   ;;; Server:
   dotspacemacs-persistent-server t ; default nil; if non-nil, advises quit functions to keep server open when quitting.

   ;;; Search Tools:
   ;; Spacemacs uses the first installed tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep") ; default '("ag" "pt" "ack" "grep")
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
This function is called immediately after `dotspacemacs/init', before layer configuration.
It is mostly useful for variables that must be set before packages are loaded. If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;;; Set up the modeline and frame title.
  (setq-default mode-line-format nil) ; Hide modeline until it is properly formatted.
  (defvar my-buffer-modified-string
    '(:eval (cond
             (buffer-read-only "🔒")
             ((buffer-modified-p) "◆")
             (t " ")))
    "Show whether the buffer has been modified since its last save.")
  (put 'my-buffer-modified-string 'risky-local-variable t)

  (defvar my-buffer-or-file-name-string
    '(:eval (if buffer-file-name buffer-file-name buffer-name))
    "The filename if there is one; otherwise, the buffer name")
  (put 'my-buffer-or-file-name-string 'risky-local-variable t)

  (defvar my-vc-string
    '(:eval (when (and vc-mode buffer-file-name)
              (let ((branch (vc-working-revision buffer-file-name))
                    (color (pcase (vc-state buffer-file-name)
                             (`up-to-date "#cccccc")
                             (`added "#99cc99")
                             (`edited "#bbdaff")
                             (`needs-merge "#ffc58f")
                             (`removed "#ff9da4")
                             (`ignored "#999999")
                             (_ nil))))
                (propertize branch 'face `(:foreground ,color)))))
    "The branch of a version-controlled file, colored to indicate status")
  (put 'my-vc-string 'risky-local-variable t)

  (defun my-refresh-all-modelines ()
    (force-mode-line-update t))
  (add-hook 'magit-refresh-buffer-hook 'my-refresh-all-modelines)

  (defun my-style-modeline ()
    (if (string= (buffer-name) "*spacemacs*")
        (setq mode-line-format nil) ; Hide mode line on start page.
      (setq mode-line-format
            (list
             " %[" ; Show recursive editing.
             "%b%" ; buffer
             " "
             my-buffer-modified-string
             "%]  " ; Show recursive editing.
             "(%l,%c)  " ; (line,column)
             mode-name ; major mode
             "  "
             my-vc-string ; branch
             ))))
  (add-hook 'after-change-major-mode-hook 'my-style-modeline)
  (add-hook 'buffer-list-update-hook 'my-style-modeline)

  (defun my-style-frame-title ()
    (setq frame-title-format
          (list
           my-buffer-or-file-name-string
           " "
           my-buffer-modified-string
           )))
  (add-hook 'after-change-major-mode-hook 'my-style-frame-title)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
Except for variables that should be set before a package is loaded, put your configuration code here."
  (set-face-font 'variable-pitch "Adobe Garamond Pro-14")
  (global-hl-line-mode -1) ; Disable current line highlight.
  (global-visual-line-mode) ; Always wrap lines to window.
  (setq-default major-mode 'text-mode) ; Use text instead of fundamental.
  (add-hook 'text-mode-hook 'variable-pitch-mode)
  (add-hook 'prog-mode-hook 'adaptive-wrap-prefix-mode) ; Indent wrapped lines in source code.
  (add-hook 'prog-mode-hook 'linum-mode) ; Show line numbers in source code.
  (add-hook 'prog-mode-hook 'rainbow-mode) ; Color color strings like "#4971af" in source code.

  (setq vc-follow-symlinks t)

  ;; Navigate wrapped lines:
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (define-key evil-insert-state-map (kbd "<backspace>") 'evil-delete-backward-word) ; Make backspace delete the whole word.

  ;;; Mouse copy stuff:
  (setq mouse-drag-copy-region t)
  (setq kill-do-not-save-duplicates t) ; Don't copy identical text twice.
  (setq-default read-quoted-char-radix 16) ; Use hex for unicode character input.

  ;;; Elisp:
  (add-hook 'emacs-lisp-mode-hook 'paren-face-mode) ; Fade parentheses in elisp mode.
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

  ;;; Elm:
  (defun my-elm-mode-hook ()
    "elm setup adapted from http://www.lambdacat.com/post-modern-emacs-setup-for-elm/"
    (setq company-backends '(company-elm))
    (elm-oracle-setup-completion))
  (add-hook 'elm-mode-hook 'my-elm-mode-hook)
  )

;; Do not write anything past this comment. This is where Emacs will auto-generate custom variable definitions.
