;;; emacs/init.el -*- lexical-binding: t; no-byte-compile: t; coding: utf-8-unix; -*-

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref nil :depth 1 :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca--activate-package)))

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

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca-wait) ;; Ensure Elpaca bootstrap finishes before continuing
;; use-package support for Elpaca
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor :ensure t)

(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup)
  (general-create-definer tree/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

(tree/leader-keys
  "." '(find-file :wk "Find file")
  "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config (org)")
  "TAB TAB" '(comment-line :wk "Comment lines")
  "h" '(:ignore t :wk "Help")
  "h f" '(describe-function :wk "Describe function")
  "h v" '(describe-variable :wk "Describe variable")
  "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config (init.el)"))

(tree/leader-keys
  "b"  '(:ignore t :wk "Buffers")
  "b b" '(switch-to-buffer :wk "Switch buffer")
  "b i" '(ibuffer :wk "Ibuffer")
  "b k" '(kill-this-buffer :wk "Kill buffer")
  "b n" '(next-buffer :wk "Next buffer")
  "b p" '(previous-buffer :wk "Previous buffer")
  "b r" '(revert-buffer :wk "Reload buffer"))

(tree/leader-keys
  "e" '(:ignore t :wk "Evaluate")
  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
  "e e" '(eval-expression :wk "Evaluate an elisp expression")
  "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
  "e r" '(eval-region :wk "Evaluate elisp in region")))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions
	'(("gif" . "gimp")
	  ("jpg" . "gimp")
	  ("png" . "gimp")
	  ("mkv" . "vlc")
	  ("mp4" . "vlc")
	  ("kra" . "krita"))))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package org-auto-tangle
  :ensure t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(setq org-directory "~/org/")

(require 'org-tempo)

(defun dashboard-banner-title ()
  "Set a colorful title for the dashboard banner."
  (propertize "TREEMACS Really Whoops The Unicorn's Ass!"))
(custom-set-variables)
(custom-set-faces
 '(dashboard-banner-logo-title ((t (:inherit default :foreground "spring green")))))

(use-package dashboard
   :ensure t 
   :init
   (setq initial-buffer-choice 'dashboard-open)
   (setq dashboard-set-heading-icons t)
   (setq dashboard-set-file-icons t)
   (setq dashboard-banner-logo-title (dashboard-banner-title))
   ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
   (setq dashboard-startup-banner "~/.config/emacs/.images/splash.png")  ;; use custom image as banner
   (setq dashboard-center-content t) ;; set to 't' for centered content
   (setq dashboard-items '((recents . 5)
			   (agenda . 5 )
			   (bookmarks . 3)
			   (projects . 3)
			   (registers . 3)))
   :custom 
   (dashboard-modify-heading-icons '((recents . "file-text")
				       (bookmarks . "book")))
   :config
   (dashboard-setup-startup-hook))

;; Install and configure Neotree
(use-package neotree
  :ensure t
  :after dashboard
  :config
  (setq neo-window-width 35
        neo-window-fixed-size nil
        neo-smart-open t
        neo-autorefresh t
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-window-position 'left)

  ;; Track if Neotree has opened once
  (defvar my/dashboard-neotree-opened nil
    "Prevent Neotree from reopening after the dashboard initializes once.")

  ;; Hook: Open Neotree after Dashboard loads, only once
  (add-hook 'dashboard-after-initialize-hook
            (lambda ()
              (unless my/dashboard-neotree-opened
                (let ((root (or (ignore-errors (project-root (project-current)))
                                user-emacs-directory)))
                  (neotree-show)
                  (neotree-dir root)
                  (other-window 1)) ;; Return focus to dashboard
                (setq my/dashboard-neotree-opened t)))))

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key [C-wheel-up] 'text-scale-increase)
(global-set-key [C-wheel-down] 'text-scale-decrease)
