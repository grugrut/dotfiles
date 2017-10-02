;;; init.el --- My first loaded init script

;; Author: grugrut
;; URL: https://github.com/grugrut/.emacs.d/init.el

;;; Commentary:

;;; Code:

(prefer-coding-system 'utf-8-unix)

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(package-refresh-contents)

(use-package init-loader
  :ensure t
  :init
  (setq init-loader-byte-compile t)
  :config
  (init-loader-load (concat user-emacs-directory "loader-files")))
;;; init.el ends here

