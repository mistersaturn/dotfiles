;;; $DOOMDIR/config.el -*- lexical-binding: t; no-byte-compile: t; coding: utf-8-unix; -*-

(beacon-mode 1)

(setq org-directory "~/org/")

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

(setq dired-open-extensions '(("gif" . "gimp")
                              ("jpg" . "gimp")
                              ("png" . "gimp")
                              ("mkv" . "vlc")
                              ("mp4" . "vlc")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(use-package emojify
  :hook (after-init . global-emojify-mode))

(setq display-line-numbers-type t)

(set-face-attribute 'mode-line nil :font "EnvyCodeR Nerd Font 13")
(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(map! :leader
      :desc "Switch to perspective NAME"       "DEL" #'persp-switch
      :desc "Switch to buffer in perspective"  "," #'persp-switch-to-buffer
      :desc "Switch to next perspective"       "]" #'persp-next
      :desc "Switch to previous perspective"   "[" #'persp-prev
      :desc "Add a buffer current perspective" "+" #'persp-add-buffer
      :desc "Remove perspective by name"       "-" #'persp-remove-by-name)

(setq doom-theme 'doom-tomorrow-night)
