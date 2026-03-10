(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
                                        ;
;This is only needed once, near the top of the file
(eval-when-compile
;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "~/.config/emacs/lisp/")
  (require 'use-package))


;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" "~/src/config/emacs"))
(require 'init-packages)
(require 'custom-functions)

(setq customize-file (expand-file-name "customize.el" (file-name-directory load-file-name)))
(load customize-file) 
