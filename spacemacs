;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     ;; company-mode
     ;; dash
     emacs-lisp
     git
     haskell
     javascript
     latex
     ;; markdown
     org
     ;; powerline
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     ;; slime
     syntax-checking
     version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         spacemacs-light
                         spacemacs-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13.0 ; Use floating point for point size; integer for pixel.
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  (setq-default spacemacs-mode-line-minor-modesp nil)
)

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (global-hl-line-mode -1) ; Disable current line highlight
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode) ; Match indentation levels and comments with wrapped lines.
  (global-visual-line-mode) ; Always wrap lines to window.
  ;; Navigate wrapped lines.
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (add-hook 'prog-mode-hook 'linum-mode) ; Show line numbers for code.

  ;; Set up the modeline and frame title. Right now this overrides, but does not disable, the powerline.
  (defvar my-buffer-modified-string
    '(:eval (cond (buffer-read-only "🔒")
                  ((buffer-modified-p) "◆")
                  (t " ")))
    "Use in the modeline to show whether the buffer has been modified since its last save.")
  (put 'my-buffer-modified-string 'risky-local-variable t)
  (defvar my-buffer-or-file-name-string
    '(:eval (if buffer-file-name buffer-file-name
              buffer-name))
    "Show the filename if there is one; otherwise, the buffer name.")
  (put 'my-buffer-or-file-name-string 'risky-local-variable t)
  (defvar my-vc-string
    '(eval (when (and vc-mode buffer-file-name)
      (let ((backend (vc-backend buffer-file-name)))
        (when backend
          (format "%s / %s" backend (vc-working-revision buffer-file-name backend))))))
    ;; '(:eval (let ((backend symbol-name (vc-backend (buffer-file-name)))
    ;;               (substring vc-mode (+ (length backend) 2)))))
    "Strip backend from vc-mode. Courtesy of https://github.com/lunaryorn/blog/blob/master/posts/make-your-emacs-mode-line-more-useful.md ")
  (put 'my-vc-string 'risky-local-variable t)
  (defun my-style-modeline ()
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
            my-vc-string ;; version control state (broken)
  )))
  (add-hook 'after-change-major-mode-hook 'my-style-modeline)
  (defun spacemacs//restore-powerline (ignored_value)
    "Replace the built-in restore-powerline function"
    (my-style-modeline)) 

  (defun my-style-frame-title ()
    (setq frame-title-format
          (list
           my-buffer-or-file-name-string ;; file location
           " "
           my-buffer-modified-string
  )))
  (add-hook 'after-change-major-mode-hook 'my-style-frame-title)

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
  (defun k-swap-kill-region (click)
    "Replace region with the second element of the kill ring. (The first element will be a copy of the region.)"
    (interactive "e")
    (let ((es (event-start) click))
      (select-window (posn-window es))
      (if (and (use-region-p)
               (<= (region-beginning) (posn-point es) (region-end)))
          (progn
            (kill-region)
            (insert (current-kill 1 t)))
        (insert current-kill 0 t)))
  )

  (global-set-key (kbd "<mouse-2>") 'k-swap-kill-region)

;  (defun k-swap-kill-region ()
;                                        ;  (if mouse-drag-copy-region ))
;    (interactive e)
;    (current-kill 1 t) ; Get the second kill-ring entry.
;    )
   ;; (defun paste-or-swap (event)
   ;;   "If clicking in the region, swap region with previously yanked text; otherwise, paste yanked text at clicked point."
   ;;   (interactive "e")
   ;;   (let ((es (event-start event)))
   ;;     (select-window (posn-window es))
   ;;     (if (and mark-active (<= (region-beginning) (posn-point es) (region-end)))
   ;;         <kill and paste>)))
)

