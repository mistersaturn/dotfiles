(setq package-enable-at-startup nil)

(add-hook 'after-init-hook
          (lambda ()
            ;; Disable UI elements
            (scroll-bar-mode -1)
            (menu-bar-mode -1)
            (tool-bar-mode -1)
            (fringe-mode 10)

            ;; Theme
            (load-theme 'deeper-blue t)

            ;; Font settings
            (set-face-attribute 'default nil
                                :family "EnvyCodeR Nerd Font"
                                :weight 'bold
                                :height 158)

            (message "UI customization applied")))

(setq inhibit-startup-screen t)  ;; Don't show default Emacs splash
