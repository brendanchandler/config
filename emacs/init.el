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

(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(define-prefix-command 'filesystem-map)
(define-key global-map (kbd "C-c f") filesystem-map)

(define-prefix-command 'search-map)
(define-key global-map (kbd "C-c j") search-map)

(use-package emacs
  :bind
  ("C-j" . avy-goto-char)
  ("C-c j j" . avy-goto-char)
  ("C-o" . other-window)
  ("C-c f s" . save-buffer)
  ("C-c f g" . rgrep)
  ("C-c f f" . find-file)
  ("C-c f d" . dired)
  ("C-c f i" . (lambda () (interactive)
		 (find-file user-init-file)))
  ("C-c r i" . (lambda () (interactive)
		 (load-file user-init-file)))
  ("C-c m e" . hippie-expand)
  ("C-c j l" . avy-goto-line)
  ("C-c j i" . imenu)
  ("C-c b b" . switch-to-buffer)
  ("C-c p f" . projectile-find-file)
  ("C-c p p" . projectile-switch-project)
  ("C-c p s" . projectile-grep)
  ("C-c p c" . projectile-compile-project)
  ("C-c p d" . projectile-dired)
  ("C-c x i" . (lambda () (interactive)
		 (load-file user-init-file)))
  ("C-c c a" . toggle-current-window-dedication)
  :config
  (windmove-default-keybindings 'control)
)

(use-package avy
:bind
("C-;" . avy-goto-char-timer)
  ("C-c j j" . avy-goto-char-timer)
  ("C-c j l" . avy-goto-line))

(use-package markdown-mode)

(use-package which-key
  :init
  (which-key-mode t))

(use-package magit)

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/src/"
					 "~/C2/iocs/"
					 "~/C2/devel/")))
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))
  
(use-package cc-mode)

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

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)))

(defun bc/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-all-windows nil)
 '(custom-enabled-themes '(leuven))
 '(dired-dwim-target 'dired-dwim-target-next)
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(mermaid-mode which-key vertico use-package projectile orderless markdown-mode magit god-mode evil avy))
 '(projectile-project-root-functions
   '(projectile-root-local projectile-root-top-down-recurring projectile-root-bottom-up projectile-root-top-down))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
