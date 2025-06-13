;;; custom-functions.el --- Custom functions for my emacs usage

;; Author: Brendan Chandler <bchandler@anl.gov>
;; Maintainer: Brendan Chandler <bchandler@anl.gov>
;; URL: n/a
;; Version: 0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Contains defuns and other custom utilities for my editor config

;;; Code:

(provide 'custom-functions)



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


(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

  (defun bc/insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; Treat underscores as part of a word, not a word separator.
;; Lets evil * and other movement keys work as expected
(modify-syntax-entry ?_ "w")



;;; custom-functions.el ends here
