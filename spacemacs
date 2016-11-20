;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup. It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
This function should only set values."
  (setq-default
   ;; Base setup, a layer contained in the directory `+distribution':
   ;; Available distributions are `spacemacs-base' and `spacemacs'.
   dotspacemacs-distribution 'spacemacs-base ; default 'spacemacs

   ;; Package downloading and retention:
   ;; Possible values are `used', `used-but-keep-unused' and `all'.
   ;; * `used' will download only explicitly used packages and remove any unused packages as well as their dependencies.
   ;; * `used-but-keep-unused' will download only used packages but won't delete them if they become unused.
   ;; * `all' will download all the packages regardless of whether they are used or not, and packages won't be deleted by Spacemacs.
   dotspacemacs-download-packages 'used ; default 'used

   ;;; Layer installation & uninstalling:
   ;; * `all' installs all supported packages and never uninstalls them.
   ;; * `used-only' installs only explicitly used packages and uninstalls others.
   ;; * `used-but-keep-unused' installs only explicitly used packages but keeps others.
   dotspacemacs-install-packages 'used-only ; default 'used-only

   ;;; Deferred layer installation
   ;; Delay layer installation until opening a file with a supported type. Layers will be added to `dotspacemacs-configuration-layers' when they are installed.
   ;; * `unused' will wait to install layers not listed in  `dotspacemacs-configuration-layers'.
   ;; * `all' will wait to install any layer that supports lazy installation, even those listed in `dotspacemacs-configuration-layers'.
   ;; * `nil' disables lazy installation.
   dotspacemacs-enable-lazy-installation 'unused ; default 'unused
   ;; Will Spacemacs ask before lazily installing layers?
   dotspacemacs-ask-for-lazy-installation t ; default t

   ;; Additional paths for configuration layers:
   ;; Paths must have a trailing slash (e.g. `~/.mycontribs/').
   dotspacemacs-configuration-layer-path '() ; default '()

   ;; Configuration layers to load:
   dotspacemacs-configuration-layers
   '(
     (colors :packages
             rainbow-mode ; for color strings only
             :variables
             rainbow-x-colors nil
             rainbow-html-colors nil)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spacemacs-editing
     spacemacs-evil ; This pulls in a lot of packages; figure out a way to pare them down.
     themes-megapack
     ;;; Bindings:
     better-defaults
     vinegar ; dired
     ;;; Checking & Completion:
     auto-completion
     ;; helm ; Use ivy instead.
     ivy
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     flyspell-sort-corrections nil)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil)
     ;;; Languages:
     elm
     emacs-lisp
     haskell
     (html :variables ; for CSS
           web-mode-css-indent-offset 2
           web-mode-enable-css-colorization nil ; already done with colors
           )
     javascript
     markdown
     vimscript
     ;;; VC:
     git
     github
     version-control
     )

   ;; Packages installed without being wrapped in a layer:
   ;; If you need configuration for these packages, consider creating a layer. You can also put the configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     ;; (acme-mouse :location (recipe :fetcher github :repo "akrito/acme-mouse")) ; does not work in Spacemacs.
     adaptive-wrap
     aggressive-indent
     cl-lib ; cl- prefixed lisp functions
     company
     dash ; list functions
     paren-face
     ;; popwin ; so helm [space] b b works (not using Helm).
     )

   ;; Packages that will not be updated:
   dotspacemacs-frozen-packages '() ; default '()

   ;; Packages and extensions that will not be installed or loaded:
   dotspacemacs-excluded-packages
   '(
     fancy-battery ; The GUI shell shows this.
     highlight-indentation ; Indentation shows this.
     highlight-parentheses ; Use `paren-face-mode' instead.
     powerline ; Use customized modeline instead.
     ;; rainbow-delimiters ; Use `paren-face-mode'.
     spray ; Not currently using spacemacs for speed reading.
     vi-tilde-fringe ; Line numbers show this.
     )
   ))

(defun dotspacemacs/init ()
  "Initialization function:
This function is called at the very startup of Spacemacs initialization before layers configuration. You should not put any user code in here besides modifying the variable values."
  ;;; Spacemacs settings:
  (setq-default
   ;;; ELPA
   ;; Will ELPA repositories be contacted via HTTPS? Disable only if you have no way to use HTTPS. Launching Emacs with the parameter `--insecure' sets this variable to nil.
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
   ;; This is disabled when the current branch is `develop'.
   dotspacemacs-check-for-update t ; default t

   ;; Editing style:
   ;; One of `vim', `emacs' or `hybrid'. `hybrid' is like `vim' except that `insert state' is replaced by the `hybrid state' with `emacs' key bindings. The value can also be a list with `:variables' keyword (similar to layers). Check the editing styles section of the documentation for details on available variables.
   dotspacemacs-editing-style 'vim ; default 'vim

   ;; Will Spacemacs output loading progress to the `*Messages*' buffer?
   dotspacemacs-verbose-loading nil ; default nil
   ;; Will Spacemacs display a progress bar is displayed when loading? This may increase the boot time; set it to nil to boost the loading time.
   dotspacemacs-loading-progress-bar nil ; default t

   ;; The startup banner:
   ;; * `official' displays the official spacemacs logo.
   ;; * `random' chooses a random text banner in `core/banners' directory.
   ;; * An integer value is the index of text banner.
   ;; * A string value must be a path to an image format supported by your Emacs build.
   ;; * If nil then no banner is displayed.
   dotspacemacs-startup-banner nil ; default 'official
   ;; Items to show in startup buffer:
   ;; A list or an association list of of the form `(list-type . list-size)`. If nil it is disabled. Possible values for list-type are: `recents' `bookmarks' `projects' `agenda' `todos'.
   dotspacemacs-startup-lists '((recents . 7)
                                (projects . 7))
   ;; Will the startup buffer resize?
   dotspacemacs-startup-buffer-responsive t ; default t

   ;; Default major mode of the scratch buffer:
   dotspacemacs-scratch-mode 'text-mode ; default 'text-mode

   ;; Themes:
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

   ;; Will the cursor color match the state color in GUI Spacemacs?
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts:
   ;; `powerline-scale' provides tweaking of the mode-line size to make separators look better.
   dotspacemacs-default-font
   '(
     "Source Code Pro"
     :size 13.0
     :weight normal
     :width normal
     :powerline-scale 1.0
     )

   ;; The leader key:
   dotspacemacs-leader-key "SPC"
   ;; The command key used for Vim Ex commands (ex-commands):
   dotspacemacs-ex-command-key ":"
   ;; Major mode leader key:
   ;; Equivalent to pressing `<leader> m`. Disabled when nil.
   dotspacemacs-major-mode-leader-key "," ; default ","
   ;; The leader key in `emacs state' and `insert state':
   dotspacemacs-emacs-leader-key "M-m" ; default "M-m"
   ;; The key used for Emacs commands (M-x) after pressing on the leader key:
   dotspacemacs-emacs-command-key "SPC" ; default "SPC"
   ;; Major mode leader key accessible in `emacs state' and `insert state':
   dotspacemacs-major-mode-emacs-leader-key "C-M-m" ; default "C-M-m"

   ;;; Vim keybindings
   ;; Will `Y' be remapped to `y$'?
   dotspacemacs-remap-Y-to-y$ t ; default t
   ;; Will the shift mappings `<' and `>' maintain visual state?
   dotspacemacs-retain-visual-state-on-shift t ; default t
   ;; Will J and K move lines up and down when in visual mode?
   dotspacemacs-visual-line-move-text nil ; default nil
   ;; Will the meaning of `g' be inverted in `:substitute' Evil ex-commands?
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
   ;; * `original' auto-saves the file in-place.
   ;; * `cache' auto-saves the file to another file stored in the cache directory.
   ;; * `nil' disables auto-saving.
   dotspacemacs-auto-save-file-location 'cache ; default 'cache

   ;;; Maximum number of rollback slots to keep in the cache:
   dotspacemacs-max-rollback-slots 5 ; default 5

   ;;; Helm
   ;; Will `helm' will try to minimize its size?
   dotspacemacs-helm-resize nil ; default nil
   ;; Will the helm header be hidden when there is only one source?
   dotspacemacs-helm-no-header t ; default nil
   ;; The position of `helm':
   ;; Options are `bottom', `top', `left', or `right'.
   dotspacemacs-helm-position 'bottom ; default 'bottom
   ;; Fuzzy matching in helm:
   ;; If set to `always', force fuzzy matching in all non-asynchronous sources. If set to `source', preserve individual source settings. Else, disable fuzzy matching in all sources.
   dotspacemacs-helm-use-fuzzy 'always ; default 'always

   ;; Paste micro-state:
   ;; Will `p' cycle through the kill ring content?
   dotspacemacs-enable-paste-transient-state t ; default nil

   ;; Which-key delay in seconds:
   ;; The which-key buffer is a popup listing the commands bound to the current keystroke sequence.
   dotspacemacs-which-key-delay 0.4 ; default 0.4

   ;; Which-key frame position:
   ;; Possible values are `right', `bottom' and `right-then-bottom'. `right-then-bottom' tries to display the frame to the right; if there is insufficient space it displays it at the bottom.
   dotspacemacs-which-key-position 'bottom ; default 'bottom

   ;; Should `switch-to-buffer' put the buffer in a same-purpose window even if the buffer can be put in the current window?
   dotspacemacs-switch-to-buffer-prefers-purpose t; default nil

   ;;; Fullscreen
   ;; Will Spacemacs start up fullscreen? (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil ; default nil
   ;; Will `spacemacs/toggle-fullscreen' use non-native fullscreen? Use to disable fullscreen animations in OSX.
   dotspacemacs-fullscreen-use-non-native nil ; default nil
   ;; Will the frame be maximized when Spacemacs starts up? Ignored if `dotspacemacs-fullscreen-at-startup' is not nil. (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil ; default nil

   ;;; Frame opacity
   ;; Transparency can be toggled through `toggle-transparency'.
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
   ;; * `t' turns on line numbers in all `prog-mode' and `text-mode' derivatives.
   ;; * `relative' turns on relative line numbers also.
   ;; * `nil' disables line numbers.
   dotspacemacs-line-numbers nil ; default nil

   ;; Code folding:
   ;; Possible values are `evil' and `origami'.
   dotspacemacs-folding-method 'evil ; default 'evil

   ;; Scope for highlighting delimiters:
   ;; Possible values are `any', `current', `all' or `nil'. `all' highlights any scope and emphasizes the current one.
   dotspacemacs-highlight-delimiters 'current ; default nil

   ;;; Smartparens
   ;; Will `smartparens-strict-mode' be enabled in `prog-mode'?
   dotspacemacs-smartparens-strict-mode nil ; default nil
   ;; Will `)' in insert mode pass over any automatically added closing parenthesis, bracket, quote, etc.? This can be temporary disabled by pressing `C-q' before `)'.
   dotspacemacs-smart-closing-parenthesis nil ; default nil

   ;; Whitespace cleanup on save:
   ;; * `all' aggressively deletes empty lines and long sequences of whitespace.
   ;; * `trailing' deletes only the whitespace at end of lines.
   ;; * `changed' deletes only whitespace for changed lines.
   ;; * `nil' disables cleanup.
   dotspacemacs-whitespace-cleanup 'trailing ; default nil

   ;; Server:
   ;; Will quit functions be advised to leave the server running?
   dotspacemacs-persistent-server t ; default nil

   ;; Search Tools:
   ;; Spacemacs uses the first installed tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep") ; default '("ag" "pt" "ack" "grep")
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
This function is called immediately after `dotspacemacs/init', before layer configuration. It is mostly useful for variables that must be set before packages are loaded. If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (setq my-default-mode-line mode-line-format) ; Save in case you want to know.

  (customize-set-variable 'adaptive-fill-regexp "[ \t]*\\([-–!|#%;>·•‣⁃◦]+[ \t]*\\)*") ; Removed '*' so I can make non-unicode bullet lists. Ideally there should be two separate variables: adaptive-fill-regexp and adaptive-indent-regexp. The first would indent with the 'whitespace' character, but the second would indent with actual whitespace.

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization, after layers configuration. Put your configuration code--except for variables that should be set before a package is loaded--here."

  ;;; ----------------------------------------------
  ;;; Mode Line, Header Line, and Frame Title Format

  (defvar my-buffer-modified-string
    '(:eval
      (if (and buffer-read-only buffer-file-truename)
          (if (buffer-modified-p)
              (propertize "◆🔒"
                          'help-echo "Modified read-only file ‑ click to save a copy."
                          'local-map (make-mode-line-mouse-map 'mouse-1 #'write-file))
            (propertize "🔒"
                        'help-echo "Read-only file ‑ click to save a copy."
                        'local-map (make-mode-line-mouse-map 'mouse-1 #'write-file)))
        (if (buffer-modified-p)
            (if buffer-file-truename
                (propertize "◆"
                            'help-echo "Modified file ‑ click to save."
                            'local-map (make-mode-line-mouse-map 'mouse-1 #'save-buffer))
              (propertize "◆"
                          'help-echo "Modified buffer ‑ click to save as a file."
                          'local-map (make-mode-line-mouse-map 'mouse-1 #'write-file)))
          " ")))
    "Show whether the buffer has been modified since its last save; click to save. Should 'do what I mean'.")
  (put 'my-buffer-modified-string 'risky-local-variable t)

  (defvar my-buffer-name-string
    '(:eval
      (if buffer-file-truename
          (propertize (buffer-name)
                      'help-echo (abbreviate-file-name buffer-file-truename)
                      'local-map (make-mode-line-mouse-map
                                  'mouse-1 (lambda () (interactive)
                                             (dired (file-name-directory buffer-file-truename)))))
        (buffer-name)))
    "The name of the buffer. If it's a file, shows the directory on hover and opens dired with a click.")
  (put 'my-buffer-name-string 'risky-local-variable t)

  (defvar my-buffer-or-file-name-string
    '(:eval (if buffer-file-truename
                (file-name-nondirectory buffer-file-truename)
              (buffer-name)))
    "The filename if there is one; otherwise, the buffer name")
  (put 'my-buffer-or-file-name-string 'risky-local-variable t)

  (defvar my-file-directory-string
    '(:eval (when buffer-file-truename
              (file-name-directory (abbreviate-file-name buffer-file-truename))))
    "The directory of the current file.")
  (put 'my-file-directory-string 'risky-local-variable t)

  (defvar my-point-string
    '(:eval (propertize "(%l, %c)"
                        'help-echo "Toggle line numbers."
                        'local-map (make-mode-line-mouse-map 'mouse-1 #'linum-mode)))
    "The row and column coordinates of the point.")
  (put 'my-point-string 'risky-local-variable t)

  (defvar my-major-mode-name
    '(:eval (propertize (mode-name)
                        'local-map (make-mode-line-mouse-map 'mouse-1 #'describe-mode)))
    "The buffer's major-mode")
  (put 'my-major-mode-name 'risky-local-variable t)

  (defvar my-vc-string
    '(:eval
      (when (and vc-mode buffer-file-name)
        (let ((branch (vc-working-revision buffer-file-name))
              (desc.color (pcase (vc-state buffer-file-name)
                            (`up-to-date '("up to date" . "#cccccc"))
                            (`added '("staged" . "#99cc99"))
                            (`edited '("unstaged" . "#bbdaff"))
                            (`needs-merge '("needs to be merged" . "#ffc58f"))
                            (`removed '("removed" . "#ff9da4"))
                            (`ignored '("ignored" . "#999999"))
                            (_ '(nil . nil)))))
          (propertize branch
                      'face `(:foreground ,(cdr desc.color))
                      'help-echo (concat "Magit status: " (car desc.color))
                      'local-map (make-mode-line-mouse-map 'mouse-1 #'magit-status)))))
    "The branch of a version-controlled file, colored to indicate status")
  (put 'my-vc-string 'risky-local-variable t)

  (defun my-format-prog-mode-line ()
    (setq mode-line-format
          (list
           " "
           my-buffer-modified-string
           " "
           my-buffer-name-string
           "  "
           my-point-string
           "  "
           mode-name
           "  "
           my-vc-string
           )
          ))

  (defun my-format-text-mode-line ()
    (setq mode-line-format
          (list
           " "
           my-buffer-modified-string
           " "
           my-buffer-name-string
           "  "
           my-vc-string
           )
          ))

  (defun my-format-frame-title ()
    (when (display-graphic-p)
      (setq frame-title-format
            (list
             my-buffer-modified-string
             " "
             my-file-directory-string
             my-buffer-or-file-name-string
             "  "
             ))))
  (my-format-frame-title)

  ;;; --------------------------------
  ;;; Fonts

  (defun my-select-font (fonts)
    (cond ((null fonts) (face-attribute 'default :family))
          ((member (car fonts) (font-family-list)) (car fonts))
          (t (my-select-font (cdr fonts)))))

  (set-face-attribute
   'fixed-pitch nil
   :family (my-select-font
            '("Source Code Pro"
              "IBM 3720"
              "DejaVu Sans Mono"
              "Monaco"
              "Lucida Console")))

  (set-face-attribute
   'variable-pitch nil
   :family (my-select-font
            '("ET Book"
              "ETBembo"
              "Bembo Book MT Std"
              "Bembo MT Book Std"
              "Garamond Premier Pro"
              "Garamond Premr Pro"
              "Adobe Garamond Expert"
              "Garamond")))

  (defun my-reset-font-height-by-platform ()
    (let ((h (if (string= system-type "gnu/linux") 148 120)))
      (mapc (lambda (face) (set-face-attribute face nil :height h))
            '(default fixed-pitch variable-pitch ))))
  (my-reset-font-height-by-platform)

  ;;; ---------------------------------
  ;;; Miscelaneous Global Stuff

  (global-hl-line-mode -1) ; Disable current line highlight.
  (global-visual-line-mode 1) ; Always wrap lines to window.
  (setq-default major-mode 'text-mode) ; Use text instead of fundamental.
  (setq vc-follow-symlinks t)

  ;;; ----------------------------------
  ;;; Hooks

  ;; * first-change-hook is called immediately before changing an unmodified buffer.
  ;; * after-change-major-mode-hook
  ;; * buffer-list-update-hook
  ;; * magit-refresh-buffer-hook

  (defun my-add-hooks (mode-hooks hook-functions)
    "Add all hook-functions to all made-hooks."
    (dolist (mode-hook mode-hooks)
      (dolist (hook-function hook-functions)
        (add-hook mode-hook hook-function))))

  (my-add-hooks
   '(
     after-change-major-mode-hook
     buffer-list-update-hook
     first-change-hook
     )
   '(
     force-mode-line-update
     ))

  (my-add-hooks
   '(prog-mode-hook)
   '(
     adaptive-wrap-prefix-mode ; Indent wrapped lines in source code.
     rainbow-mode ; Color color strings like "#4971af" in source code.
     my-format-prog-mode-line
     ))

  (my-add-hooks
   '(text-mode-hook)
   '(
     variable-pitch-mode
     my-format-text-mode-line
     ))

  ;; Hide the mode-line when not needed useful.
  (my-add-hooks
   '(
     help-mode-hook
     magit-mode-hook
     spacemacs-buffer-mode-hook
     )
   '(
     (lambda () (setq mode-line-format nil))
     ))

  (add-hook 'magit-refresh-buffer-hook
            (lambda () (force-mode-line-update t)))

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
  (mapcar (lambda (x)
            (global-set-key
             (kbd (if (string= system-type "gnu/linux")
                      (cadr x)
                    (caddr x)))
             (car x)))
          '((my-zoom-in "C-<mouse-4>"  "C-<wheel-up>")
            (my-zoom-out "C-<mouse-5>" "C-<wheel-down>")))

  ;;; Insert unicode character with Ctrl Shift u.
  (global-set-key (kbd "C-S-u") 'insert-char)

  ;;; Use hex for unicode character input.
  (setq-default read-quoted-char-radix 16)

  ;;; Mouse & copy / paste / delete
  (setq mouse-drag-copy-region t ; Copy on select -- disable for acme-mouse.
        kill-do-not-save-duplicates t ; Don't copy identical text twice.
        )

  ;;; ---------------------------
  ;;; Major Mode Configurations

  ;;; Git
  ;; Use spacemacs for editing git commit messages.
  (global-git-commit-mode t)

  ;;; Elisp:
  (my-add-hooks
   '(emacs-lisp-mode-hook)
   '(
     aggressive-indent-mode
     paren-face-mode ; Fade parentheses.
     ))

  ;;; Elm:
  ;; (defun my-elm-mode-hook ()
  ;;   "elm setup adapted from http://www.lambdacat.com/post-modern-emacs-setup-for-elm/"
  ;;   (add-to-list company-backends '(company-elm))
  ;;   (elm-oracle-setup-completion))
  ;; (add-hook 'elm-mode-hook 'my-elm-mode-hook)


  ;;; ---------------------------------------
  ;;; Lastly, some hackish theming:
  ;;; The main point is to, as much as possible without being distracting, distinguish stuff that does stuff from stuff that does not do stuff and things that look similar and act differently.

  (defun max-color-val ()
    (car (color-values "white")))

  (defun my-color-values-to-string (c)
    "Create a color string from and Emacs numerical color triplet."
    (let* ((color-ratio (/ (max-color-val) 255))
           (r (truncate (car c) color-ratio))
           (g (truncate (cadr c) color-ratio))
           (b (truncate (caddr c) color-ratio)))
      (format "#%02X%02X%02X" r g b)))

  (defun my-adaptive-shadow-face ()
    "Create a string representation of a color halfway between the foreground and the background."
    (let*
        ((default-foreground
           (color-values (face-attribute 'default :foreground)))
         (default-background
           (color-values (face-attribute 'default :background))))
      (my-color-values-to-string
       (-zip-with (lambda (x y) (/ (+ x y) 2))
                  default-foreground
                  default-background))))

  (set-face-attribute 'shadow nil :foreground (my-adaptive-shadow-face))

  (defun my-laser-minor-theme (&optional color)
    "Add borders to the mode-line and disable its background color."
    (interactive)
    (let* ((c
            (if color color
              (face-attribute 'font-lock-comment-face :foreground)))
           (line-style `(:color ,c :style line)))
      (set-face-attribute 'mode-line nil :box nil :background nil
                          :underline c
                          :overline c)
      (set-face-attribute 'window-divider nil :foreground c)))

  (defun my-material-minor-theme ()
    "Remove borders from the mode-line when its background is different from the buffer's."
    (interactive)
    (unless (equal (face-attribute 'default :background)
                   (face-attribute 'mode-line :background))
      (set-face-attribute 'mode-line nil :box nil :underline nil :overline nil)))

  (custom-set-faces
   ;; Things that don't do stuff:
   '(font-lock-comment-face ((t (:background nil :slant normal))))
   ;; '(font-lock-comment-delimiter-face ((t (:slant normal :inherit font-lock-comment-face))))
   '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
   '(fringe ((t (:background nil :inherit font-lock-comment-face))))
   '(linum ((t (:background nil :foreground nil :inherit font-lock-comment-face))))
   '(mode-line ((t (:inherit font-lock-comment-face))))
   ;; Things that do stuff:
   ;; '(font-lock-builtin-face ((t (:inherit default))))
   ;; '(font-lock-constant-face ((t (:inherit default))))
   ;; '(font-lock-keyword-face ((t (:foreground nil :inherit default))))
   ;; '(font-lock-type-face ((t (:inherit default))))
   '(font-lock-function-name-face ((t (:foreground nil :inherit default))))
   '(font-lock-variable-name-face ((t (:foreground nil :inherit default))))
   ;; Things that look like other things:
   '(font-lock-string-face ((t (:slant italic))))
   )
  )

;; Do not write anything past this comment. This is where Emacs will auto-generate custom variable definitions.
