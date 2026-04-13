(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-all-windows nil)
 '(backup-directory-alist '((".*" . "~/emacs-backup")))
 '(c-basic-offset 4)
 '(c-default-style "stroustrup")
 '(calendar-intermonth-header "Wk")
 '(calendar-intermonth-text
   '(propertize
     (format "%2d"
             (car
              (calendar-iso-from-absolute
               (calendar-absolute-from-gregorian (list month day year)))))
     'font-lock-face 'font-lock-function-name-face))
 '(calendar-week-start-day 1)
 '(cursor-type t)
 '(custom-enabled-themes '(modus-vivendi-tinted))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(display-line-numbers t)
 '(evil-default-state 'insert)
 '(gmm-tool-bar-style 'gnome t)
 '(gud-pdb-command-name "/C2/conda/envs/aux/bin/python3 -m pdb")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(mode-require-final-newline t)
 '(package-selected-packages
   '(avy clang-format cmake-mode company flycheck lsp-mode magit
         orderless rust-mode vertico))
 '(python-fill-docstring-style 'pep-257)
 '(python-indent-def-block-scale 4)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-guess-indent-offset-verbose nil)
 '(require-final-newline t)
 '(show-paren-style 'parenthesis)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight regular :height 98 :width normal)))))

