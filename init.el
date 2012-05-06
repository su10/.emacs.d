;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

(setq user-full-name "su10")
(setq user-mail-address "okkotonushi_sama@yahoo.co.jp")

;; commandキーをMetaキーに割り当て(参考:http://weblog.nekonya.com/2010/11/cocoa-emacs-command-meta.html)
(cond
 ((string-match "apple-darwin" system-configuration)
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote supper))
  )
)
;; C-hをバックスペースに
(keyboard-translate ?\C-h ?\C-?)
;; ヘルプコマンドをC-x ?に割り当て
(global-set-key (kbd "C-x ?") 'help-command)
;; C-mで改行と同時にインデント
(global-set-key (kbd "C-m") 'newline-and-indent)
;; "C-t"でウィンドウ切り替え
(global-set-key (kbd "C-t") 'other-window)
;; タブキーで半角スペース２つ入力
(setq-default indent-tabs-mode nil tab-width 2)
;; カラム番号を表示
(column-number-mode t)
;; 行番号を常に表示
(dolist (hook (list
              'c-mode-hook
              'emacs-lisp-mode-hook
              'lisp-interaction-mode-hook
              'lisp-mode-hook
              'java-mode-hook
              'sh-mode-hook
              ))
(add-hook hook (lambda () (linum-mode t))))
;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")
;; バックアップとオートセーブファイルを~/.emacs.d/backupsへ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/buckups"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; 常時デバッグ状態
;(setq debug-on-error t)

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Emacs Lisp のPathを通す
(add-to-load-path "elisp"
                  "conf"
                  "public_repos")
;; 表示テーマの設定
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-hober))

;; 文字コード
(set-language-environment 'Japanese)
;(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)


;; 
;;(require 'init_global)
;;(require 'init_key)
;;(require 'init_common)
;; 環境依存設定
;;(cond
;; (mac-p (require 'init_cocoa_main))
 ;(carbon-p (require 'init_carbon_main))
 ;(ns-p (require 'init_ntemacs_main))
;; )
