;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs-base

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/local/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '((ivy :packages
          (not helm-make)) ; This may be misguided, but why???
     (auto-completion :packages
                      (not helm-company
                           helm-c-yasnippet
                           smartparens))
     (colors :packages
             rainbow-mode
             :variables
             rainbow-x-colors nil
             rainbow-html-colors nil)
     emacs-lisp
     (git :packages
          (not helm-gitignore))
     markdown
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;;     evil-cleverparens
     (parinfer :variables
               parinfer-auto-switch-indent-mode t
               parinfer-extensions
               '(defaults
                  evil
                  paredit
                  smart-yank
                  smart-tab))
     (spacemacs-completion :packages
                           default-ivy-config)
     (spacemacs-editing :packages
                        (not aggressive-indent
                             clean-aindent
                             lorem-ipsum
                             smartparens))
     (spacemacs-navigation :packages  ; renamed from `spacemacs-ui'
                           (not golden-ratio))
     (spacemacs-visual :packages
                       (not fill-column-indicator))
     spell-checking
     syntax-checking
     version-control
     vinegar) ; Simplified/improved `dired'

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(adaptive-wrap
     paren-face)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(helm orgit)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark leuven)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font `("Source Code Pro"
                               :size ,(if (string= system-type "gnu/linux")
                                          14.0
                                        12.0)
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t

   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t

   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil

   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands.
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil

   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil

   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom

   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always

   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p'
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'any

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil

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
   dotspacemacs-frame-title-format nil

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))


(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; (require 'parinferlib "~/.emacs.d/private/local/parinfer-elisp/parinferlib"
  (setq debug-on-error t)
  (setq use-dialog-box nil)
  ;; https://github.com/syl20bnr/spacemacs/issues/5435
  ;; This problem may be related with wid-edit.el and mouse-1-click-follows-link. Now I can avoid automatic yank by putting the following config in spacemacs/user-config.
  (add-hook 'spacemacs-buffer-mode-hook
            (lambda ()
              (set (make-local-variable 'mouse-1-click-follows-link) nil))))


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

;;; In order of dependencies...
  (defun my-require-or-quelpa (package-symbol)
    "Use a copy of the package in the private/local/ dir, or if it's not there, get it with `quelpa'."
    (let* ((name (symbol-name package-symbol))
           (package-path (concat "~/.emacs.d/private/local/"
                                 name "/" name ".el")))
      (if (file-readable-p package-path)
          (require package-symbol package-path)
        (quelpa package-symbol
                :fetcher 'github
                :repo (concat "wygulmage/" name ".el"))
        (message "Fetched %s with quelpa." name))))
  (mapc #'my-require-or-quelpa
        [umr
         miscellaneous
         hook-up
         primary-pane
         fac
         statusbar
         minor-theme])

;;; Statusbar
  (let ((c (or (plist-get (face-attribute 'mode-line :box) :color)
               (face-attribute 'shadow :foreground))))
    (seq-doseq (face [vertical-border border window-divider])
      (set-face-attribute face nil :foreground c :background c)))

  (set-frame-parameter nil 'bottom-divider-width 1)
  ;; (add-to-list 'default-frame-alist '(bottom-divider-width 1))
  ;; (add-to-list 'default-frame-alist '(right-divider-width 1))

  (setq-default mode-line-format statusbar-base-layout)
  (setq mode-line-format statusbar-base-layout) ; just in case.

  ;; Statusbar faces
  (defun my-theme-tweaks ()
    "Tweak faces to simplify themes. Requires `fac', `minor-theme', and `statusbar'"
    ;; Faces of things that that don't do stuff:
    (set-face-attribute 'font-lock-comment-face nil
                        :background 'unspecified
                        :slant 'normal)
    (seq-doseq (face [font-lock-doc-face
                      fringe
                      linum])
      (set-face-attribute face nil
                          :foreground 'unspecified
                          :background 'unspecified
                          :inherit 'font-lock-comment-face))
    ;; Faces of things that do stuff:
    (seq-doseq (face [font-lock-keyword-face
                      font-lock-function-name-face
                      font-lock-variable-name-face])
      (set-face-attribute face nil
                          :foreground 'unspecified
                          :background 'unspecified
                          :inherit 'default))
       ;;; Things that look like other things:
    (set-face-attribute 'font-lock-string-face nil
                        :slant 'italic)

    (fac-fade-foreground 'shadow 'default)
    (fac-fade-foreground 'font-lock-comment-delimiter-face 'font-lock-comment-face)
    (if (and (display-graphic-p) (fboundp 'minor-theme-flat))
        nil ; (minor-theme-flat)
      (minor-theme-laser)))


  (my-theme-tweaks)
  (hook-up [after-load-theme-hook] [my-theme-tweaks])


;;; Try to keep to 80 columns.
  ;; FIXME: Does not work well with `linum-mode'.
  (defun my-80-column-display ()
    "Make the text are 80 columns."
    (interactive)
    (let ((new-margins
           (max 1
                (floor (- (window-total-width)
                          (or left-fringe-width 0)
                          (or right-fringe-width 0)
                          81)
                       2))))
       (set-window-margins nil new-margins new-margins)))

  ;; Somehow this isn't an infinite loop:
  (defun add-local-hook-for-80-column-display ()
      (add-hook 'window-configuration-change-hook
                #'my-80-column-display nil t))

  ;; Use 80 columns in normal editing modes:
  (hook-up [text-mode-hook prog-mode-hook]
           [add-local-hook-for-80-column-display])

  ;; Convince `evil' that the window is wide enough to split:
  (defun my-zero-window-margins ()
    (set-window-margins nil 0 0))
  (hook-up-make-hook :before evil-window-vsplit
              my-zero-window-margins)

;;; Miscellaneous Global Stuff
  (global-hl-line-mode -1) ; Disable current line highlight.
  (global-visual-line-mode 1) ; Always wrap lines to window.
  (setq
   kill-do-not-save-duplicates t ; Don't copy identical text twice.
   vc-follow-symlinks t) ; Always follow symlinks to version-controlled files.

  (hook-up
   [prog-mode-hook]
   [adaptive-wrap-prefix-mode ; Indent wrapped lines in source code.
    rainbow-mode ; Color color strings like "#4971af" in source code.
    statusbar-use-prog-mode-layout])

  ;; Hide the mode-line when not needed useful.
  (hook-up
   [dired-mode-hook
    help-mode-hook
    magit-mode-hook
    ranger-mode-hook
    spacemacs-buffer-mode-hook]
   [statusbar-hide])


;;; Key Maps

  ;; Ignore mouse-wheel left and right.
  (misc--def-keys global-map
    "<mouse-6>" #'ignore
    "<mouse-7>" #'ignore)

  ;; Navigate help buffers.
  (when (string= system-type "windows-nt")
    (misc--def-keys help-mode-map
      "<mouse-4>" #'help-go-back ; Windows mouse back (Linux mouse wheel up)
      "<mouse-5>" #'help-go-forward)) ; Windows mouse forwards

  ;; Navigate wrapped lines.
  (misc--def-keys evil-normal-state-map
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line)

  ;; Zoom with Ctrl + mouse wheel.
  (apply #'misc--def-keys global-map
         (misc--alternate
          (if (string= system-type "gnu/linux")
              '("C-<mouse-4>" "C-<mouse-5>") ; Linux mouse wheel
            '("C-<wheel-up>" "C-<wheel-down>")) ; Windows mouse wheel
          '(spacemacs/scale-up-font spacemacs/scale-down-font)))

  ;; Toggle comment with SPC ;.
  (defun my-comment-dwim ()
    "If the region is not active, select the current line. Then, if the region is a comment, uncomment it, and otherwise comment it out."
    (interactive)
    (let ((start) (end))
      (if (region-active-p)
          (setq start (region-beginning)
                end (region-end))
        (setq start (line-beginning-position)
              end (line-end-position)))
      (comment-or-uncomment-region start end)))

  (spacemacs/set-leader-keys
    ";" #'my-comment-dwim)


;;; Languages

;;;; Lisps
  (setq-default lisp-minor-modes
                [paren-face-mode ; Dim parentheses.
                 parinfer-mode]) ; Manage parentheses automagically.

;;;; Emacs-Lisp
  (hook-up
   [emacs-lisp-mode-hook]
   lisp-minor-modes)

;;;; Sh
  (add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
