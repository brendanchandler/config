(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "~/.config/emacs/lisp/")
  (require 'use-package))


(define-prefix-command 'filesystem-map)
(define-key global-map (kbd "C-c C-f") filesystem-map)

(define-prefix-command 'search-map)
(define-key global-map (kbd "C-c C-j") search-map)

(define-prefix-command 'o-map)
(define-key global-map (kbd "C-o") o-map)

(setq tr--last-command nil)

(defun tr (command)
  "Run the specified command in the currently active tmux pane"
  (interactive "sCommand: ")
  (setq tr--last-command command)
  (call-process "tmux" nil nil nil "send-keys" command "Enter"))

(defun trr ()
  "Re-run the previous command"
  (interactive)
  (if tr--last-command
      (call-process "tmux" nil nil nil "send-keys" tr--last-command "Enter")
    (message "No available previous command!")))

(use-package org
  :bind
  ("M-h" . windmove-left)
  :config
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "M-h") #'windmove-left)))

(use-package emacs
  :ensure t
  :bind (("C-c f s" . save-buffer)
	 ("C-c f g" . rgrep)
	 ("C-c f f" . find-file)
	 ("C-c f d" . dired)
	 ("C-c f t" . (lambda () (interactive)
			(find-file "/ssh:bchandler@tesseract:/home/phoebus/BCHANDLER/")))
	 ("C-c f w" . (lambda () (interactive)
			(find-file "/ssh:bchandler@weed:/home/bchandler/")))
	 ("C-c f h" . (lambda () (interactive)
			(find-file "/ssh:bchandler@voltctl:/home/helios/BCHANDLER/")))
	 ("C-c f i" . (lambda () (interactive)
			(find-file user-init-file)))
	 ("C-c c f r" . (lambda () (interactive)
			  (load-file user-init-file)))
	 ("C-o e" . hippie-expand)
	 ("M-i" . imenu)
	 ("M-h" . windmove-left)
	 ("M-l" . windmove-right)
	 ("M-j" . windmove-down)
	 ("M-k" . windmove-up)
	 ("C-c j b" . switch-to-buffer)
	 ("C-c m" . (lambda () (interactive)
		      (occur "# section:")))
	 ("M-e" . forward-to-word)
	 ("C-x C-b" . ibuffer)
	 ("M-n" . move-line-down)
	 ("M-p" . move-line-up)
	 ("C-c w" . bc/vertical-windows)
	 )
  :config
  (windmove-default-keybindings)
  (global-unset-key (kbd "C-z"))
  
  (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
  (defun my-goto-match-beginning ()
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))

  (defadvice isearch-exit (after my-goto-match-beginning activate)
    "Go to beginning of match."
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))

  )

;; (use-package clipetty--dcs-end
;;   :ensure t
;;   :config
;;   (clipetty-mode))


(use-package avy
  :ensure t
  :bind ("M-o" . avy-goto-char))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

(use-package markdown-mode
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode t))

(use-package magit
  :ensure t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))
  
(use-package cc-mode
  :ensure t)


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
  (find-file "~/.config/emacs/init.el"))
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


(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)))

;; (use-package eglot-mode
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (c++-mode . eglot)
;; 	 (c-mode . eglot)
;; 	 (python-mode . eglot)))


(add-hook 'c-mode-hook 'eglot)
(add-hook 'c++-mode-hook 'eglot)
(add-hook 'python-mode-hook 'eglot)

(defun bc/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun bc/vertical-windows ()
  "Sets up 3 vertical, balanced windows"
  (interactive)
  (progn (delete-other-windows)
       (split-window-right)
       (split-window-right)
       (balance-windows)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-all-windows nil)
 '(backup-directory-alist '((".*" . "~/emacs-backup")))
 '(c-default-style "stroustrup")
 '(custom-enabled-themes '(leuven))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(display-line-numbers t)
 '(gmm-tool-bar-style 'gnome t)
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(package-selected-packages
   '(clipetty--dcs-end clipetty company lsp-treemacs flycheck which-key vertico use-package orderless markdown-mode magit avy))
 '(projectile-project-root-functions
   '(projectile-root-local projectile-root-top-down-recurring projectile-root-bottom-up projectile-root-top-down))
 '(python-fill-docstring-style 'pep-257)
 '(show-paren-style 'parenthesis)
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 90 :width normal)))))
(put 'erase-buffer 'disabled nil)
