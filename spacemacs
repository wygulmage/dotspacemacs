;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration:
This function should include only set values."
  (setq-default
   ;;; Base setup, a layer contained in the directory `+distribution':
   ;; Available distributions are `spacemacs-base' and `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs-base

   ;;; Layers installed only when a file with a supported type is opened:
   ;; Possible values are `all', `unused', and `nil'.
   ;; `unused' will lazy install only layers not listed in `dotspacemacs-configuration-layers'.
   ;; `all' will lazy install any layer that support lazy installation, even those listed in `dotspacemacs-configuration-layers'.
   ;; `nil' disables the lazy installation feature.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;;; Additional paths for configuration layers:
   ;; Paths must have a trailing slash (e.g. `~/.mycontribs/').
   dotspacemacs-configuration-layer-path '()

   ;;; Configuration layers to load:
   dotspacemacs-configuration-layers
   '(
     (colors :variables ; for color strings only
             rainbow-x-colors nil
             rainbow-html-colors nil)
     ;; org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spacemacs-editing
     themes-megapack
     ;;; Bindings:
     better-defaults
     ;; unimpaired ; paired brackets; not present in develop branch?
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
     ;;; latex ; breaks spacemacs if latex is not installed.
     markdown
     vimscript
     ;;; VC:
     git
     github
     version-control
     )

   ;;; List of additional packages that will be installed without being wrapped in a layer:
   ;; If you need configuration for these packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     adaptive-wrap
     aggressive-indent
     paren-face
     popwin ; so helm [space] b b works
     )

   ;;; Packages that cannot be updated:
   dotspacemacs-frozen-packages '()

   ;;; Packages and/or extensions that will not be installed or loaded:
   dotspacemacs-excluded-packages '(vi-tilde-fringe)

   ;;; The behaviour of Spacemacs when downloading packages:
   ;; Possible values are `used', `used-but-keep-unused' and `all'.
   ;; `used' will download only explicitly used packages and remove any unused packages as well as their dependencies.
   ;; `used-but-keep-unused' will download only used packages but won't delete them if they become unused.
   ;; `all' will download all the packages regardless of whether they are used or not, and packages won't be deleted by Spacemacs.
   ;; (default is `used'.)
   dotspacemacs-download-packages 'used
   ))

(defun dotspacemacs/init ()
  "Initialization function:
This function is called at the very startup of Spacemacs initialization before layers configuration. You should not put any user code in there besides modifying the variable
values."
  ;;; An exhaustive list of all the supported spacemacs settings:
  (setq-default
   ;; If non-nil, ELPA repositories are contacted via HTTPS whenever possible. Set to nil if you have no way to use HTTPS in your environment, otherwise set to t.
   ;; This variable has no effect if Emacs is launched with the parameter `--insecure', which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;;; Maximum allowed time in seconds to contact an ELPA repository:
   dotspacemacs-elpa-timeout 5

   ;; If non-nil then spacemacs will check for updates at startup when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t

   ;; If non-nil, a form that evaluates to a package directory. For example, to use different package directories for different Emacs versions, set this to `emacs-version'.
   ;; (default nil)
   dotspacemacs-elpa-subdirectory nil

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the `hybrid state' with `emacs' key bindings. The value can also be a list with `:variables' keyword (similar to layers). Check the editing styles section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil, output loading progress to `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;;; The startup banner:
   ;; `official' displays the official spacemacs logo.
   ;; `random' chooses a random text banner in `core/banners' directory.
   ;; An integer value is the index of text banner.
   ;; A string value must be a path to an image format supported by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   ;; (default 'official)
   dotspacemacs-startup-banner nil

   ;;; Items to show in startup buffer:
   ;; A list or an association list of of the form `(list-type . list-size)`. If nil it is disabled.
   ;; Possible values for list-type are: `recents' `bookmarks' `projects' `agenda' `todos'.
   dotspacemacs-startup-lists '((recents . 7)
                                (projects . 7))

   ;;; Default major mode of the scratch buffer (default `text-mode'):
   dotspacemacs-scratch-mode 'text-mode

   ;;; Themes:
   ;; The first of the list is loaded when spacemacs starts. Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light).
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

   ;; If non nil the cursor color matches the state color in GUI Emacs.
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
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;;; Major mode leader key:
   ;; A shortcut key which is the equivalent of pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;;; Major mode leader key accessible in `emacs state' and `insert state':
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;;; The key used for Emacs commands (M-x) (after pressing on the leader key):
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;;; The command key used for Evil commands (ex-commands) and Emacs commands (M-x):
   ;; By default the command key is `:' so ex-commands are executed like in Vim with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"

   ;;; Variables to control whether separate commands are bound in the GUI to the key pairs C-i, TAB and C-m, RET:
   ;; Setting it to a non-nil value, allows for separate commands under <C-i> and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil

   ;; If non-nil, `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t

   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t

   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil

   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command. (default nil)
   dotspacemacs-ex-substitute-global nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil, the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resume automatically on start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;;: Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues:
   ;; Opening a file as 'large' means that no major mode or minor modes are active.
   ;; (default 1)
   dotspacemacs-large-file-size 1

   ;;; Where to auto-save files:
   ;; `original' auto-saves the file in-place.
   ;; `cache' auto-saves the file to another file stored in the cache directory.
   ;; `nil' disables auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;;; Maximum number of rollback slots to keep in the cache (default 5):
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil

   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t

   ;;; The position of `helm':
   ;; Options are `bottom', `top', `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom

   ;;; Fuzzy matching in helm:
   ;; If set to `always', force fuzzy matching in all non-asynchronous sources. If set to `source', preserve individual source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always

   ;;; The paste micro-state:
   ;; When non-nil, pressing `p` several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;;; Which-key delay in seconds:
   ;; The which-key buffer is a popup listing the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;;; Which-key frame position:
   ;; Possible values are `right', `bottom' and `right-then-bottom'. `right-then-bottom' tries to display the frame to the right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; If non-nil, a progress bar is displayed when spacemacs is loading. This may increase the boot time; set it to nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil, the frame is fullscreen when Emacs starts up. (default nil) (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil, `spacemacs/toggle-fullscreen' will not use native fullscreen. Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil, the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil. (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;;; The opacity of a frame when it's active or selected (0..100):
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;;; The opacity of a frame when its inactive or deselected (0..100):
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil: show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil, show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil, unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil, smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; If non-nil, line numbers are turned on in all `prog-mode' and `text-mode' derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;;; Code folding method:
   ;; Possible values are `evil' and `origami'. (default 'evil)
   dotspacemacs-folding-method 'evil

   ;;; Scope for highlighting delimiters:
   ;; Possible values are `any', `current', `all' or `nil'. Default is `all' (highlight any scope and emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'current

   ;; If non-nil, smartparens-strict-mode will be enabled in programming modes. (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil, pressing the closing parenthesis `)' key in insert mode passes over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non-nil, advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;;; Search Tools:
   ;; Spacemacs uses the first installed tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")

   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in `dotspacemacs/user-config' first."

;;; Set up the modeline and frame title.
  (setq-default mode-line-format nil) ; Hide modeline until it is properly formatted.
  (setq-default major-mode 'text-mode) ; Use text instead of fundamental.
  (defvar my-buffer-modified-string
    '(:eval (cond
             (buffer-read-only "🔒")
             ((buffer-modified-p) "◆")
             (t " ")))
    "Use in the modeline to show whether the buffer has been modified since its last save.")
  (put 'my-buffer-modified-string 'risky-local-variable t)

  (defvar my-buffer-or-file-name-string
    '(:eval (if buffer-file-name buffer-file-name buffer-name))
    "Show the filename if there is one; otherwise, the buffer name.")
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
    "Show the branch of a version-controlled file, colored to indicate status.")
  (put 'my-vc-string 'risky-local-variable t)

  (defun my-refresh-all-modelines ()
    (force-mode-line-update t))
  (add-hook 'magit-refresh-buffer-hook 'my-refresh-all-modelines)

  (defun my-style-modeline ()
    (if (string= (buffer-name) "*spacemacs*")
        (setq mode-line-format nil)
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
           my-buffer-or-file-name-string ; file location
           " "
           my-buffer-modified-string
           )))
  (add-hook 'after-change-major-mode-hook 'my-style-frame-title)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is explicitly specified that a variable should be set before a package is loaded, you should place your code here."
  (set-face-font 'variable-pitch "Adobe Garamond Pro-14")
  (global-hl-line-mode -1) ; Disable current line highlight.
  (add-hook 'text-mode-hook 'variable-pitch-mode)
  (add-hook 'prog-mode-hook 'linum-mode) ; Show line numbers for code.
  (add-hook 'prog-mode-hook 'rainbow-mode) ; Color color strings like "#4971af".
  (setq vc-follow-symlinks t)

  ;;; Elisp:
  (add-hook 'emacs-lisp-mode-hook 'paren-face-mode) ; Fade parentheses in elisp mode.
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

  ;;; Elm autocompletion:
  (defun my-elm-mode-hook ()
    "elm setup adapted from http://www.lambdacat.com/post-modern-emacs-setup-for-elm/"
    (setq company-backends '(company-elm))
    (elm-oracle-setup-completion))
  (add-hook 'elm-mode-hook 'my-elm-mode-hook)

  (global-visual-line-mode) ; Always wrap lines to window.
  (add-hook 'prog-mode-hook 'adaptive-wrap-prefix-mode) ; Indent wrapped lines in source code.

  ;; Navigate wrapped lines:
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (define-key evil-insert-state-map (kbd "<backspace>") 'evil-delete-backward-word) ; Make backspace delete the whole word.

  ;;; Mouse copy stuff:
  (setq mouse-drag-copy-region t)
  (setq kill-do-not-save-duplicates t) ; Don't copy identical text twice.
  (setq-default read-quoted-char-radix 16) ; Use hex for unicode character input.
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
