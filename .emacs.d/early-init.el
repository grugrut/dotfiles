;;; early-init.el --- My early-init script -*- coding: utf-8 ; lexical-binding: t -*-
;; Author: grugrut <grugruglut+github@gmail.com>
;; URL:
;; Version: 1.00

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(setq debug-on-error t)

(let ((my-init-org (concat user-emacs-directory "init.org"))
      (my-init-el (concat user-emacs-directory "init.el")))
  (when (file-newer-than-file-p my-init-org my-init-el)
    (message "WARN: init.el is old.\n")))

;; ツールバーを表示しない
(tool-bar-mode 0)

;; スクロールバーを表示しない
(set-scroll-bar-mode nil)

;; 行番号を表示
(line-number-mode +1)
(column-number-mode +1)

;; 行番号表示(Emacs26以降)
(global-display-line-numbers-mode t)
(custom-set-variables '(display-line-numbers-width-start t))

;; タブ表示
(tab-bar-mode t)

;; native-compのワーニング抑制
(custom-set-variables '(warning-suppress-types '((comp))))

(setq default-frame-alist
      (append '((width                . 140)  ; フレーム幅
                (height               . 40 ) ; フレーム高
                (left                 . 170 ) ; 配置左位置
                (top                  . 30 ) ; 配置上位置
                (line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 12 ) ; 左フリンジ幅
                (right-fringe         . 12 ) ; 右フリンジ幅
                (menu-bar-lines       . 1  ) ; メニューバー
                (cursor-type          . box) ; カーソル種別
                (alpha                . 100) ; 透明度
                )
              default-frame-alist))
(setq initial-frame-alist default-frame-alist)

(custom-set-variables '(custom-file (expand-file-name "custom.el" user-emacs-directory)))

;;; early-init.el ends here
