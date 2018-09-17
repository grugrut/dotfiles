;;; init.el --- My first loaded init script

;; Author: grugrut
;; URL: https://github.com/grugrut/.emacs.d/init.el

;;; Commentary:

;;; Code:

(prefer-coding-system 'utf-8-unix)

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")))
(package-initialize)
;;(package-refresh-contents) ;;重たいので手動でやる

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


(use-package init-loader
  :ensure
  :config
  (setq init-loader-byte-compile t)
  (init-loader-load (concat user-emacs-directory "loader-files")))

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;;; init.el ends here
