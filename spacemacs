;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers
Do not put user code in this function; only modify the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs ;; Base distribution. This is a layer contained in the directory `+distribution'. For now available distributions are `spacemacs-base' and `spacemacs'. (default 'spacemacs)

   dotspacemacs-configuration-layer-path '() ;; List of additional paths where to look for configuration layers. Paths must have a trailing slash (i.e. `~/.mycontribs/').
   dotspacemacs-configuration-layers ;; List of configuration layers to load. If it is the symbol `all' instead of a list then all discovered layers will be installed.
   '(
     ;; Press <SPC f e R> (Vim style) or <M-m f e R> (Emacs style) to load a new layer.
     better-defaults
     (colors :variables ; Used only for color strings (no nyancat or rainbow identifiers).
             rainbow-x-colors nil
             rainbow-html-colors nil)
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     themes-megapack
     ;; Checking & completion:
     auto-completion
     spell-checking
     syntax-checking
     ;; Key bindings:
     unimpaired ; paired bracket bindings
     vinegar ; dired-related bindings
     ;; Languages:
     emacs-lisp
     haskell
     javascript
     latex
     ;; Version Control:
     git
     version-control
     )

   dotspacemacs-additional-packages '(paren-face) ;; List of packages that will be installed without being wrapped in a layer. If you need configuration for these packages then create a layer or put the configuration in `dotspacemacs/config' (`dotspacemacs/user-config'?).
   dotspacemacs-excluded-packages ;; A list of packages and/or extensions that will not be installed and loaded.
   '(
     fancy-battery ; The GUI shell shows this.
     highlight-indentation ; Indentation shows this.
     highlight-parentheses ; Use `paren-face-mode' instead.
     powerline ; Use customized modeline instead.
     rainbow-delimiters ; Use `paren-face-mode'.
     spray ; Not currently using spacemacs for speed reading.
     vi-tilde-fringe ; Line numbers show this.
     )
   spacemacs-delete-orphan-packages t ;; If non-nil, spacemacs will delete any orphan packages, i.e. packages that are declared in a layer which is not a member of the list `dotspacemacs-configuration-layers'. (default t)
   ))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default ;; This setq-default sexp is an exhaustive list of all the supported spacemacs settings.

   dotspacemacs-editing-style 'vim ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid' uses emacs key bindings for vim's insert mode, but otherwise leaves evil unchanged. (default 'vim)

   dotspacemacs-verbose-loading nil ;; If non-nil, output loading progress in `*Messages*' buffer. (default nil)

   dotspacemacs-startup-banner nil ;; Specify the startup banner. `official', displays the official spacemacs logo. An integer value is the index of text banner; `random' chooses a random text banner in `core/banners' directory. A string value must be a path to an image format supported by your Emacs build. If nil, no banner is displayed. (default 'official)

   dotspacemacs-loading-progress-bar nil ;; If non-nil, a progress bar is displayed when spacemacs is loading. This may increase the boot time on some systems and emacs builds, set it to nil to boost the loading time. (default t)

   dotspacemacs-startup-lists '(recents projects) ;; List of items to show in the startup buffer. If nil it is disabled. Possible values are: `recents' `bookmarks' `projects'. (default '(recents projects))

   dotspacemacs-themes ;; List of themes; the first of the list is loaded when spacemacs starts. Press <SPC> T n to cycle to the next theme in the list (works great with 2 themes variants, one dark and one light).
   '(
     sanityinc-tomorrow-eighties
     spacemacs-dark
     solarized-dark
     leuven
     monokai
     )

   dotspacemacs-colorize-cursor-according-to-state t ;; If non-nil the cursor color matches the state color.

   dotspacemacs-default-font ;; Default font. `powerline-scale' tweaks the mode-line size.
   '(
     "Source Code Pro"
     :size 13.0 ;; FP for point; Int for pixels.
     :weight normal
     :width normal
     :powerline-scale 1.1
     )

   dotspacemacs-leader-key "SPC" ;; The leader key (default "SPC").
   dotspacemacs-emacs-leader-key "M-m" ;; The leader key accessible in `emacs state' and `insert state'. (default "M-m")
   dotspacemacs-major-mode-leader-key "," ;; Major mode leader key is a shortcut equivalent to pressing `<leader> m'. Set to `nil' to disable. (default ",")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m" ;; Major mode leader key accessible in `emacs state' and `insert state'. (default "C-M-m")
   dotspacemacs-command-key ":" ;; The command key used for Evil commands (ex-commands) and Emacs commands (M-x). Emacs commands are executed with `<leader> :'. (default ":")

   dotspacemacs-remap-Y-to-y$ t ;; If non-nil, `Y' is remapped to `y$'. (default t)

   dotspacemacs-auto-save-file-location 'cache ;; Location for auto-save files. `original' auto-saves the file in-place; `cache' auto-saves the file to another file stored in the cache directory; and `nil' disables auto-saving. (default 'cache)

   dotspacemacs-use-ido nil ;; If non nil then `ido' replaces `helm' for some commands. For now only `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-helm-resize nil ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-no-header t ;; If non nil, the helm header is hidden when there is only one source. (default nil)
   dotspacemacs-helm-position 'bottom ;; define the position to display `helm', options are `bottom', `top', `left', or `right'. (default 'bottom)

   dotspacemacs-enable-paste-micro-state t ;; If non nil the paste micro-state is enabled. When enabled, pressing `p' several times cycle between the kill ring content. (default nil)

   dotspacemacs-which-key-delay 0.4 ;; Which-key delay in seconds. The which-key buffer is the popup listing the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-position 'bottom ;; which-key frame position. Possible values are `right', `bottom' and `right-then-bottom'. right-then-bottom tries to display the frame to the right; if there is not enough space it is placed at the bottom. (default 'bottom)

   dotspacemacs-fullscreen-at-startup nil ;; Emacs 24.4+ only. If non-nil, the frame is fullscreen when Emacs starts up. (default nil)
   dotspacemacs-fullscreen-use-non-native nil ;; If non-nil, `spacemacs/toggle-fullscreen' will not use native fullscreen. Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-maximized-at-startup nil  ;; Emacs 24.4+ only. If non-nil, the frame is maximized when Emacs starts up. Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil. (default nil)

   dotspacemacs-active-transparency 90 ;; A value from the range (0..100), in increasing opacity, which describes the transparency level of a frame when it's active or selected. Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90 ;; A value from the range (0..100), in increasing opacity, which describes the transparency level of a frame when it's inactive or deselected. Transparency can be toggled through `toggle-transparency'. (default 90)

   dotspacemacs-mode-line-unicode-symbols nil ;; If non-nil, unicode symbols are displayed in the mode line. (default t) -- Still uses unicode if `nil', but no longer requires powerline.

   dotspacemacs-smooth-scrolling t ;; If non-nil, smooth scrolling (native-scrolling) is enabled. Smooth scrolling overrides the default behavior of Emacs which recenters the point when it reaches the top or bottom of the screen. (default t)

   dotspacemacs-smartparens-strict-mode nil ;; If non-nil, smartparens-strict-mode will be enabled in programming modes. (default nil)
   dotspacemacs-highlight-delimiters 'current ;; The scope for highlighting delimiters. Possible values are `any',`current', `all' or `nil'. (`all' highlights any scope and emphasis the current one.) (default 'all)

   dotspacemacs-persistent-server nil ;; If non-nil, advises quit functions to keep server open when quitting. (default nil)

   dotspacemacs-search-tools '("ag" "pt" "ack" "grep") ;; List of search tool executable names. Spacemacs uses the first installed tool of the list. Supported tools are `ag', `pt', `ack' and `grep'. (default '("ag" "pt" "ack" "grep"))

   dotspacemacs-default-package-repository nil ;; The default package repository used if no explicit repository has been specified with an installed package. Not used for now. (default nil)
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any user code."
  (unless (and (fboundp 'server-running-p) (server-running-p)) (server-start)) ; Start server if it isn't started. If server.el isn't loaded, `server-running-p' won't be bound.
  (defun switch-from-scratch-to-spacemacs ()
    "If the current buffer is *scratch*, switch to *spacemacs*. Used to circumvent emacsclient defaults."
    (when (string= (buffer-name) "*scratch*") (switch-to-buffer (get-buffer "*spacemacs*"))))
  (add-hook 'after-make-frame-functions 'switch-from-scratch-to-spacemacs)
  ;; Set up the modeline and frame title.
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
              (vc-working-revision buffer-file-name)))
    "Show the branch of a version-controlled file.")
  (put 'my-vc-string 'risky-local-variable t)

  (defun my-style-modeline ()
    (if (string= (buffer-name) "*spacemacs*")
        (setq mode-line-format nil)
      (setq mode-line-format
            (list
             " %[" ;; Show recursive editing.
             "%b%" ;; buffer
             " "
             my-buffer-modified-string
             "%]  " ;; Show recursive editing.
             "(%l,%c)  " ;; (line,column)
             mode-name ;; major mode
             "  "
             my-vc-string ; branch
             ))))
  (add-hook 'after-change-major-mode-hook 'my-style-modeline)
  (add-hook 'buffer-list-update-hook 'my-style-modeline)

  (defun my-style-frame-title ()
    (setq frame-title-format
          (list
           my-buffer-or-file-name-string ;; file location
           " "
           my-buffer-modified-string
           )))
  (add-hook 'after-change-major-mode-hook 'my-style-frame-title)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after layers configuration. You are free to put any user code."
  (global-hl-line-mode -1) ; Disable current line highlight.
  (add-hook 'emacs-lisp-mode-hook 'paren-face-mode) ; Fade parentheses in elisp mode.
  (global-visual-line-mode) ; Always wrap lines to window.
  ;; Navigate wrapped lines:
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (add-hook 'prog-mode-hook 'linum-mode) ; Show line numbers for code.
  (add-hook 'prog-mode-hook 'aggressive-indent-mode) ; not sure if this is needed
  (add-hook 'prog-mode-hook 'rainbow-mode) ; colored color strings
  ;; Allow the deletion of server files (courtesy of https://superuser.com/questions/176207/emacs-daemon-not-deleting-socket ):
  (defmacro bypass-trash-in-function (f)
    "Make `f' use normal deletion, not send-to-trash."
    `(defadvice ,f (around no-trash activate)
       "Ignore `delete-by-moving-to-trash' in this function. See `bypass-trash-in-function'."
       (let (delete-by-moving-to-trash)
         ad-do-it)))
  ;; Now apply it to the server functions:
  (mapc (lambda (f) (eval `(bypass-trash-in-function ,f)))
        '(server-start server-sentinel server-force-delete))
  ;; Configure mouse/touchpad.
  ;; My left click should:
  ;; 1. Move mark to location of down-click.
  ;; 2. Move point to location of up-click.
  ;; 3. If mark =/= point, copy region to kill-ring.
  ;; This is already implimented by `mouse-set-region' when the variable `mouse-drag-copy-region' is true.
  (setq mouse-drag-copy-region t)
  (setq kill-do-not-save-duplicates t) ; Don't copy identical text twice.
  ;; My middle click should:
  ;; 1. If click is not inside region, paste first kill-ring entry at location of click.
  ;; 2. If click is inside region, delete region and paste second kill-ring entry at point.

  ;; Lastly, some hackish theming:
  (custom-set-faces
   '(font-lock-comment-face ((t (:slant normal))))
   '(font-lock-string-face ((t (:slant italic))))
   '(font-lock-keyword-face ((t (:foreground nil :inherit default))))
   '(font-lock-function-name-face ((t (:foreground nil :inherit default))))
   '(font-lock-variable-name-face ((t (:foreground nil :inherit default))))
   '(mode-line ((t (:box nil))))
   '(fringe ((t (:background nil :inherit default))))
   '(linum ((t (:background nil :foreground nil :inherit font-lock-comment-face)))))
  )
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
