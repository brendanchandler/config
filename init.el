(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Keys which cause problems in terminals
;; backspace Ctrl+?
;; tab Ctrl+i
;; linefeed Ctrl+j
;; return Ctrl+m
;; escape Ctrl+[
(require 'use-package)

(setq use-package-always-ensure t)
;;(setq shell-file-name "bash")
;;(setq shell-command-switch "-ic")

(use-package cc-mode)
(use-package ido
  :config
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-separator "    ")
  (setq ido-auto-merge-work-directories-length -1)
  (ido-mode t))
(use-package which-key
  :config
  (which-key-mode t))
(use-package magit
  :config
  (bind-key* "C-x g" 'magit-status)
  (bind-key* "C-x M-g" 'magit-dispatch-popup))
(use-package bind-key
  :config
  (bind-key* "C-c o i" 'bc-open-emacs-init)
  (bind-key* "C-c o j" 'bc-open-journal)
  (bind-key* "C-c o c" 'bc-open-c2)
  (bind-key* "C-c o n" 'bc-open-notes-dir)
  (bind-key* "C-c o s" 'bc-open-src)
  (bind-key* "C-c o l" 'bc-open-local-dir)
  (bind-key* "C-c s g" 'grep)
  (bind-key* "C-c s t" 'vc-git-grep)
  (bind-key* "C-<tab>" 'ido-switch-buffer)
  (bind-key* "C-S-o" (lambda ()
                     (interactive)
                     (other-window -1)))
  (bind-key* "C-o" 'other-window)
  (bind-key* "C-<tab>" 'bc-next-buffer)
  (bind-key* "C-<iso-lefttab>" 'bc-prev-buffer))

(use-package avy
  :config
  (bind-key* "C-j" 'avy-goto-word-or-subword-1)
  (bind-key* "C-;" 'avy-goto-char-in-line))
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(use-package company)
(use-package company-anaconda)
(use-package eglot)
(use-package markdown-mode)
(use-package semantic
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

;; C-tab was timing out on some c++ completions. It defaults to timeout of 2 seconds, but we'll change to 8.
;;(setq company-async-timeout 8)
(defun bc-next-buffer ()
  (interactive)
  "Open ido or switch buffers"
  (if (eq major-mode 'minibuffer-inactive-mode)
      (ido-next-match)
    (ido-switch-buffer)))

(defun bc-prev-buffer ()
  (interactive)
  "Switch to previous buffer"
  (if (eq major-mode 'minibuffer-inactive-mode)
      (ido-prev-match)
    (ido-switch-buffer)))

(defun bc-open-emacs-init ()
  "Open EMACS init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun bc-open-journal ()
  "Open journal.org."
  (interactive)
  (find-file "~/Documents/2021/journal.org"))
(defun bc-open-notes-dir ()
  "Open notes directory."
  (interactive)
  (dired "~/Documents/2021"))
(defun bc-open-src ()
  "Open ~/src dir."
  (interactive)
  (dired "~/src"))
(defun bc-open-c2 ()
  "Open ~/C2/iocs/."
  (interactive)
  (dired "~/C2/iocs/"))
(defun bc-open-local-dir ()
  "Open /local/bchandler/."
  (interactive)
  (dired "/local/bchandler/"))
(defun bc-duplicate-window ()
  "Close all other frames and duplicate the current one."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1))
(defun bc-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#000000" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#dfdfdf"])
 '(avy-all-windows nil)
 '(aw-scope 'frame)
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.saves")))
 '(c-default-style "stroustrup")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" default))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(fci-rule-color "#525252")
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#000000" "#80A0C2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#000000" "#A2BF8A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#000000" "#3f3f3f"))
 '(line-move-visual nil)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(imenu-anywhere markdown-mode eglot company company-anaconda anaconda-mode linum-mode flycheck which-key use-package magit doom-themes doom-modeline ace-window))
 '(show-paren-mode t)
 '(vc-annotate-background "#323334")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A2BF8A")
    (cons 40 "#bac389")
    (cons 60 "#d3c788")
    (cons 80 "#ECCC87")
    (cons 100 "#e3b57e")
    (cons 120 "#da9e75")
    (cons 140 "#D2876D")
    (cons 160 "#c88982")
    (cons 180 "#be8b98")
    (cons 200 "#B58DAE")
    (cons 220 "#b97e97")
    (cons 240 "#bd6f80")
    (cons 260 "#C16069")
    (cons 280 "#a0575e")
    (cons 300 "#804f54")
    (cons 320 "#5f4749")
    (cons 340 "#525252")
    (cons 360 "#525252")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(menu-bar-mode 1)
(toggle-scroll-bar 1)
(tool-bar-mode -1)

;;(eval-after-load "p4-mode"
;;  '(progn
;;    (define-key p4-mode-map (kbd "M-e") nil) ;; unbind M-e
;;    (define-key p4-mode-map (kbd "M-a") nil))) ;; unbind M-a
;;


(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))
(put 'set-goal-column 'disabled nil)

;; Attempt to make mouse scrolling work nicer in emacs.
;;(when (display-graphic-p)
;;  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
;;	mouse-wheel-progressive-speed nil))
;;(setq scroll-step 1
;;      scroll-margin 0
;;      scroll-conservatively 100000)
;;(put 'upcase-region 'disabled nil)

(add-hook 'dired-mode-hook 'auto-revert-mode)

;;(server-start)

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)

;; Taken from https://www.emacswiki.org/emacs/IncrementNumber
(defun bc-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
(defun bc-decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))

(tool-bar-add-item "rgrep"
		   'rgrep
		   :help "Recursively grep for files")

(define-key dired-mode-map "e" (lambda () (interactive) (eww-open-file (dired-get-file-for-visit))))

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(defun my-c++-mode-hook ()
  (c-set-offset 'inline-mode 0)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
