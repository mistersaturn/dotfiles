;;; emacs/init.el -*- lexical-binding: t; no-byte-compile: t; coding: utf-8-unix; -*-

;; Elpaca package manager bootstrap - handles downloading and initial setup
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref nil :depth 1 :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca--activate-package)))

;; Bootstrap installation logic - clones, compiles, and loads Elpaca
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process
                                 `("git" nil ,buffer t "clone"
                                   ,@(when-let* ((depth (plist-get order :depth)))
                                       (list (format "--depth=%d" depth)
                                             "--no-single-branch"))
                                   ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval"
                                        "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))

;; Ensure Elpaca processes all queued packages after Emacs initialization
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca-wait) ;; Block until Elpaca bootstrap completes

;; Enable use-package syntax support for Elpaca
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package dired-open
  :ensure t
  :config
  ;; Define associations between file extensions and external programs
  ;; Customize these based on your preferred applications
  (setq dired-open-extensions
    '(("gif" . "gimp")      ;; Open GIF images in GIMP
      ("jpg" . "gimp")      ;; Open JPEG images in GIMP  
      ("png" . "gimp")      ;; Open PNG images in GIMP
      ("xcf" . "gimp")      ;; Open GIMP's native format in GIMP
      ("mkv" . "vlc")       ;; Open MKV videos in VLC
      ("mp4" . "vlc")       ;; Open MP4 videos in VLC
      ("kra" . "krita"))))  ;; Open Krita files in Krita

(use-package all-the-icons-dired
  :ensure t
  ;; Automatically enable icons in all Dired buffers
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package emojify
  :ensure t
  ;; Enable emoji support globally after Emacs finishes initializing
  :hook (after-init . global-emojify-mode))

(use-package evil
  :ensure t
  :demand t  ;; Load immediately, don't defer
  :init
  ;; Configuration that must happen before Evil Mode loads
  (setq evil-want-integration t      ;; Allow Evil to integrate with other packages
        evil-want-keybinding nil     ;; Disable default keybindings (we'll use evil-collection)
        evil-vsplit-window-right t   ;; New vertical splits appear on the right
        evil-split-window-below t)   ;; New horizontal splits appear below
  :config
  (evil-mode 1))  ;; Enable Evil Mode globally

(use-package evil-collection
  :ensure t
  :after evil  ;; Load after Evil Mode is available
  :config
  ;; Only enable Evil keybindings for specific modes to avoid conflicts
  ;; Add more modes to this list as needed
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor :ensure t)

(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup)  ;; Enable Evil Mode integration
  
  ;; Create a leader key definer function
  ;; SPC in normal/visual modes, M-SPC everywhere else
  (general-create-definer tree/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override  ;; Override other keymaps to ensure our bindings work
    :prefix "SPC"       ;; Leader key in Evil states
    :global-prefix "M-SPC")  ;; Leader key in non-Evil contexts

  ;; Define all keybindings within the same use-package block
  ;; This ensures tree/leader-keys is available when we use it
  
  ;; Utility Keybindings - Essential operations organized under leader key
  (tree/leader-keys
    "." '(find-file :wk "Find file")  ;; Quick file access
    
    ;; Direct access to configuration file for easy editing
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) 
            :wk "Open config file")
    
    "TAB TAB" '(comment-line :wk "Comment lines")  ;; Toggle line comments
    
    ;; Help system access - Emacs has excellent built-in documentation
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")    ;; Function documentation
    "h v" '(describe-variable :wk "Describe variable")    ;; Variable documentation
    
    ;; Reload configuration without restarting Emacs
    "h r r" '((lambda ()
                (interactive)
                (load-file "~/.config/emacs/init.el")
                (ignore (elpaca-process-queues)))
              :wk "Reload config"))

  ;; Buffer Keybindings - Buffer management commands under 'b' prefix
  (tree/leader-keys
    "b"  '(:ignore t :wk "Buffers")
    "b b" '(switch-to-buffer :wk "Switch buffer")      ;; Interactive buffer switching
    "b i" '(ibuffer :wk "Ibuffer")                     ;; Advanced buffer management interface
    "b k" '(kill-this-buffer :wk "Kill buffer")        ;; Close current buffer
    "b m" '(bookmark-set :wk "Set bookmark")           ;; Create bookmark at current location
    "b n" '(next-buffer :wk "Next buffer")             ;; Cycle to next buffer
    "b p" '(previous-buffer :wk "Previous buffer")     ;; Cycle to previous buffer  
    "b r" '(revert-buffer :wk "Reload buffer"))        ;; Refresh buffer from disk
    
  ;; Evaluation Keybindings - Elisp code evaluation for interactive development
  (tree/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")                    ;; Run entire buffer
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")     ;; Run function definition
    "e e" '(eval-expression :wk "Evaluate an elisp expression")            ;; Interactive evaluation
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")   ;; Run expression before cursor
    "e r" '(eval-region :wk "Evaluate elisp in region"))                   ;; Run selected region

  ;; Toggle Keybindings - Various toggles for things like line numbers and truncated lines
  (tree/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")           ;; Toggles line numbers on/off
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))                ;; Toggles line wrapping on/off

  ;; Window Keybindings - Enables navigating buffers like a window manager
  (tree/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")))

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)  ;; Enable globally
  :config
  ;; Customize the appearance and behavior of the which-key popup
  (setq which-key-side-window-location 'bottom    ;; Show popup at bottom
        which-key-sort-uppercase-first nil        ;; Sort alphabetically  
        which-key-add-column-padding 1            ;; Add spacing between columns
        which-key-max-display-columns nil         ;; Use as many columns as needed
        which-key-min-display-lines 6             ;; Minimum popup height
        which-key-side-window-slot -10            ;; Window priority
        which-key-side-window-max-height 0.25     ;; Max 25% of frame height
        which-key-idle-delay 0.8                  ;; Show popup after 0.8 seconds
        which-key-max-description-length 25       ;; Truncate long descriptions
        which-key-allow-imprecise-window-fit t    ;; Allow flexible sizing
        which-key-separator " 󰋇  "))              ;; Visual separator between key and description

(use-package git-timemachine
 :ensure t
 :after git-timemachine
 ;; Set up Evil-friendly keybindings when entering git-timemachine-mode
 :hook (evil-normalize-keymaps . git-timemachine-hook)
 :config
   ;; Vim-like navigation through Git history
   (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
   (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package hl-todo
  :ensure t
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package marginalia
 :after vertico  ;; Load after Vertico completion framework
 :ensure t
 :custom
 ;; Use detailed annotations by default, fall back to lighter ones if needed
 (setq marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light)) 
 :init
 (marginalia-mode))  ;; Enable globally

;; Nerd Icons provide beautiful file type and mode icons
(use-package nerd-icons
  :ensure t)

;; Custom color scheme for the modeline
(custom-set-faces
 '(mode-line ((t (:background "#282c34" :foreground "#abb2bf" :box nil))))
 '(mode-line-inactive ((t (:background "#1c1f24" :foreground "#5c6370" :box nil)))))

;; Custom face for buffer name section
(defface treemacs-modeline-buffer-namecol
  '((t (:background "#423f78" :foreground "#87e884" :inherit bold)))
  "Face for the Treemacs modeline buffer name color."
  :group 'treemacs)

;; Custom face for major mode section  
(defface treemacs-modeline-major-mode-namecol
  '((t (:background "#3357d3" :foreground "#83e0d0" :inherit bold)))
  "Face for the Treemacs modeline major mode color."
  :group 'treemacs)

;; Format buffer name with padding
(defun treemacs-modeline--buffer-name ()
  (format " %s " (buffer-name)))

;; Format major mode with appropriate icon and cleaned name
(defun treemacs-modeline--major-mode ()
  (let ((icon (or (nerd-icons-icon-for-mode major-mode)
          (nerd-icons-faicon "nf-fa-file_text_o"))) ;; fallback icon
    (name (capitalize (string-remove-suffix "-mode" (symbol-name major-mode)))))
    (format " %s  %s " icon name)))

;; Create flexible spacing for right-aligned elements
(defun treemacs-modeline--fill-right (reserve)
  "Return empty space leaving RESERVE space on the right."
  (propertize " "
      'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

;; Format current time and date
(defun treemacs-modeline--clock ()
  "Return formatted time string."
  (format-time-string " %a %b %d  %I:%M %p"))

;; Define modeline segments as buffer-local variables
(defvar-local treemacs-modeline-buffer-name
  '(:eval (propertize (treemacs-modeline--buffer-name)
          'face 'treemacs-modeline-buffer-namecol)))

(defvar-local treemacs-modeline-major-mode
  '(:eval (propertize (treemacs-modeline--major-mode)
          'face 'treemacs-modeline-major-mode-namecol)))

;; Assemble the complete modeline format
(setq-default mode-line-format
      '("%e"  ;; Error indicator
        "      "  ;; Visual separator
        treemacs-modeline-buffer-name
        " 󰚟 "  ;; Icon separator
        treemacs-modeline-major-mode
        "   "
        mode-line-position  ;; Cursor position info
        "  "
        vc-mode  ;; Version control status
        ;; Dynamic padding for right-aligned clock
        (:eval (treemacs-modeline--fill-right 28))
        ;; Right-aligned clock
        (:eval (treemacs-modeline--clock))))

;; Update the clock every minute
(run-at-time t 60 (lambda () (force-mode-line-update t)))

;; Mark custom variables as safe for buffer-local use
(put 'treemacs-modeline-buffer-name 'risky-local-variable t)
(put 'treemacs-modeline-major-mode 'risky-local-variable t)

(use-package org-auto-tangle
  :ensure t
  :defer t  ;; Only load when Org mode is used
  ;; Enable auto-tangling in all Org mode buffers
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-bullets
  :ensure t
  ;; Enable pretty bullets in all Org mode buffers
  :hook (org-mode . org-bullets-mode))

;; Set default directory for Org files - customize this path as needed
(setq org-directory "~/org/")

;; Disable electric indentation globally - prevents unwanted auto-indenting
(electric-indent-mode -1)
;; Ensure code blocks start at column 0 for proper execution
(setq org-edit-src-content-indentation 0)

;; Enable template expansion (e.g., <s TAB for source blocks)
(require 'org-tempo)

;; Define custom title text for the dashboard
(defun dashboard-banner-title ()
  "Set a colorful title for the dashboard banner."
  (propertize "TREEMACS Really Whoops The Unicorn's Ass!"))

;; Set custom colors for the dashboard title
(custom-set-variables)
(custom-set-faces
 '(dashboard-banner-logo-title ((t (:inherit default :foreground "#87e884")))))

(use-package dashboard
  :ensure t 
  :init
  ;; Replace default startup screen with dashboard
  (setq initial-buffer-choice 'dashboard-open)
  
  ;; Enable icons for headings and files (requires nerd-icons/all-the-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  
  ;; Set custom title
  (setq dashboard-banner-logo-title (dashboard-banner-title))
  
  ;; Banner options: use custom image instead of default Emacs logo
  ;;(setq dashboard-startup-banner 'logo) ;; uncomment for standard Emacs logo
  (setq dashboard-startup-banner "~/.config/emacs/.images/splash.png")
  
  (setq dashboard-center-content t) ;; Center all content horizontally
  
  ;; Configure dashboard sections and their item counts
  (setq dashboard-items '((recents . 5)      ;; 5 recent files
              (agenda . 5 )      ;; 5 upcoming agenda items  
              (bookmarks . 3)    ;; 3 bookmarks
              (projects . 3)     ;; 3 recent projects
              (registers . 3)))  ;; 3 stored registers
  :custom 
  ;; Customize section icons
  (dashboard-modify-heading-icons '((recents . "file-text")
                    (bookmarks . "book")))
  :config
  ;; Initialize dashboard after Emacs starts
  (dashboard-setup-startup-hook))

;; File tree sidebar for IDE-like experience
(use-package neotree
  :ensure t
  :after dashboard
  :config
  ;; Configure Neotree appearance and behavior
  (setq neo-window-width 15              ;; Sidebar width in characters
        neo-window-fixed-size nil        ;; Allow resizing  
        neo-smart-open t                 ;; Auto-expand to current file
        neo-show-hidden-files t          ;; Show dotfiles
        neo-autorefresh t                ;; Auto-update when files change
        neo-theme (if (display-graphic-p) 'nerd-icons 'ascii)  ;; Use icons in GUI
        neo-window-position 'left        ;; Position sidebar on left
        neo-mode-line-type 'neotree)     ;; Custom modeline for tree

  ;; State tracking to prevent reopening Neotree multiple times
  (defvar my/dashboard-neotree-opened nil
    "Prevent Neotree from reopening after the dashboard initializes once.")

  ;; Auto-open Neotree when dashboard loads (once per session)
  (add-hook 'dashboard-after-initialize-hook
            (lambda ()
              (unless my/dashboard-neotree-opened
                ;; Determine appropriate root directory (project or config directory)
                (let ((root (or (ignore-errors (project-root (project-current)))
                                user-emacs-directory)))
                  (neotree-show)      ;; Open the tree
                  (neotree-dir root)  ;; Set root directory
                  (other-window 1))   ;; Return focus to dashboard
                (setq my/dashboard-neotree-opened t)))))

(use-package sudo-edit
 :ensure t
 :config
  (tree/leader-keys
    "fu" '(sudo-edit-find-file :wk "Sudo find file")
    "fU" '(sudo-edit :wk "Sudo edit file")))

;; Use system trash instead of permanent deletion - safety first!
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

;; Move backup files to trash instead of cluttering directories  
(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files/")))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
         ;; Vim-like navigation in completion list
         ("C-j" . vertico-next)      ;; Move down in completions
         ("C-k" . vertico-previous)  ;; Move up in completions  
         ("C-f" . vertico-exit)      ;; Accept current selection
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))  ;; Better word deletion
  :custom
  (vertico-cycle t)  ;; Allow wrapping from bottom to top and vice versa
  :init
  (vertico-mode)  ;; Enable globally
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)) ;; Removes path when shadowing

;; Keyboard shortcuts for text scaling
(global-set-key (kbd "C-=") 'text-scale-increase)    ;; Make text larger
(global-set-key (kbd "C--") 'text-scale-decrease)    ;; Make text smaller

;; Mouse wheel support for text scaling (with Ctrl modifier)
(global-set-key [C-wheel-up] 'text-scale-increase)    ;; Ctrl + scroll up = bigger text
(global-set-key [C-wheel-down] 'text-scale-decrease)  ;; Ctrl + scroll down = smaller text
