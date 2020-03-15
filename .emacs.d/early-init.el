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

(message "Early-init start")


;; GC
(custom-set-variables '(gc-cons-threshold (* 2048 1024 1024))
                      '(gc-cons-percentage 0.6)
                      '(garbage-collection-messages t))

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

(setq default-frame-alist
      (append '((width                . 100)  ; フレーム幅
                (height               . 40 ) ; フレーム高
                (left                 . 170 ) ; 配置左位置
                (top                  . 30 ) ; 配置上位置
                (line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 12 ) ; 左フリンジ幅
                (right-fringe         . 12 ) ; 右フリンジ幅
                (menu-bar-lines       . 1  ) ; メニューバー
                ;; (tool-bar-lines       . 1  ) ; ツールバー
                ;; (vertical-scroll-bars . 1  ) ; スクロールバー
                ;; (scroll-bar-width     . 17 ) ; スクロールバー幅
                (cursor-type          . box) ; カーソル種別
                (alpha                . 100) ; 透明度
                )
              default-frame-alist))
(setq initial-frame-alist default-frame-alist)

(modify-frame-parameters nil '((sticky . t) (width . 100) (height . 40))) ; Xを使う場合の高速化設定らしい

;; (defun reset-frame-parameter (frame)
;;   (sleep-for 0.1)
;;   (set-frame-parameter frame 'height 32)
;;   (message "Set frame height to 32"))
;; (add-hook 'after-make-frame-functions #'reset-frame-parameter)

(custom-set-variables
 '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("gnu"   . "https://elpa.gnu.org/packages/"))))
(package-initialize)

(message "Early-init end")

;;; early-init.el ends here
