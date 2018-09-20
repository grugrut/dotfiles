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

;; ベンチマーク
(use-package benchmark-init
  :ensure t
  :disabled t
  :config
  ;; To disable collection of benchmark data after init is done.
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Xを使う場合の高速化設定らしい
(modify-frame-parameters nil '((wait-for-wm . nil)))

(use-package init-loader
  :ensure
  :config
  (setq init-loader-byte-compile t)
  (init-loader-load (concat user-emacs-directory "loader-files")))

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;;; init.el ends here
