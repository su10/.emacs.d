(setq user-full-name "su10")
(setq user-mail-address "su10.hatena@gmail.com")

;====================================
;フレーム位置設定(ウィンドウ） 
;====================================
(setq initial-frame-alist
      (append
       '((top . 0)    ; フレームの Y 位置(ピクセル数)
     (left . 0)    ; フレームの X 位置(ピクセル数)
   (width . 197)    ; フレーム幅(文字数)
    (height . 60)   ; フレーム高(文字数)
    ) initial-frame-alist))

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

;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は~/.emacs.d/auto-install
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

;; 表示テーマの設定
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-hober))

;; 文字コード
(set-language-environment 'Japanese)
;(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

;; commandキーをMetaキーに割り当て
;; (参考:http://weblog.nekonya.com/2010/11/cocoa-emacs-command-meta.html)
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
;; C-tでウィンドウ切り替え
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
             (cons "." "~/.emacs.d/backups"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; メニューバーを消す
(menu-bar-mode 0)
;; ツールバーを消す
(tool-bar-mode 0)
;; 対応する括弧を光らせる
(show-paren-mode t)
;; ウィンドウ内に収まりきらないときだけ括弧内も光らせる
(setq show-paren-style 'mixed)
;; 現在行を目立たせる
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "gray13"))
    (((class color)
      (background light))
     (:background "#CC0066"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode t)
(require 'col-highlight)
(column-highlight-mode 1)
(custom-set-faces
 '(col-highlight ((t (:background "gray13")))))
;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;; 最終行に必ず一行挿入する
(setq require-final-newline t)
;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-comletion-ignore-case t)
;; 補完可能なものを随時表示
(icomplete-mode t)
;; 履歴を大きくする
(setq history-length 10000)
;; ミニバッファの履歴を保存する
;(savehist-mode t)
;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 10000)
;; ファイルの先頭に#!...があるファイルを保存するとき実行権を付与
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;; 自動でスペルチェック
(setq-default flyspell-mode t)
(setq ispell-dictionary "american")
;; スクロールを一行ずつにする
(setq scroll-step 1)
;; スクロールバーを右側に表示する
(set-scroll-bar-mode 'right)
;;長い行を折り返さない
(setq-default truncate-lines t)
(setq-default truncate-partical-width-windows t)


;;; 矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil) ; そのままだと C-x が切り取りになってしまったりするので無効化

;;(require 'init_global)
;;(require 'init_key)
;;(require 'init_common)
