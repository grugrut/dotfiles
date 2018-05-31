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
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


(use-package init-loader
  :ensure t
  :init
  (setq init-loader-byte-compile t)
  :config
  (init-loader-load (concat user-emacs-directory "loader-files")))

(setq custom-file "custom.el")

;;; init.el ends here
