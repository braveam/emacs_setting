;;;; -*- mode: emacs-lisp; coding: iso-2022-7bit -*-
;;;;
;;;; Copyright (C) 2001 The Meadow Team

;; Author: Koichiro Ohba <koichiro@meadowy.org>
;;      Kyotaro HORIGUCHI <horiguti@meadowy.org>
;;      Hideyuki SHIRAI <shirai@meadowy.org>
;;      KOSEKI Yoshinori <kose@meadowy.org>
;;      and The Meadow Team.


;; ;;; Mule-UCS の設定
;; ;; ftp://ftp.m17n.org/pub/mule/Mule-UCS/ が オフィシャルサイトですが、
;; ;; http://www.meadowy.org/~shirai/elisp/mule-ucs.tar.gz に既知のパッチ
;; ;; をすべて適用したものがおいてあります。
;; ;; (set-language-environment) の前に設定します
;; (require 'jisx0213)


;;; 日本語環境設定
;(set-language-environment "Japanese")
(set-language-environment 'Japanese)
;(set-terminal-coding-system 'utf-8)
;(setq file-name-coding-system 'utf-8)
;(set-clipboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
;(setq coding-system-for-read 'mule-utf-8-unix)
(prefer-coding-system 'utf-8) ; テキストファイル
(set-default-coding-systems 'utf-8) ; dired mode のファイル名とか？
;(set-keyboard-coding-system 'utf-8)
;(set-buffer-file-coding-system 'utf-8-unix)


;;; IMEの設定
;(mw32-ime-initialize)
;(setq default-input-method "MW32-IME")
;(setq-default mw32-ime-mode-line-state-indicator "[--]")
;(setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
;(add-hook 'mw32-ime-on-hook
;	  (function (lambda () (set-cursor-height 2))))
;(add-hook 'mw32-ime-off-hook
;	  (function (lambda () (set-cursor-height 4))))


;; ;;; カーソルの設定
;; ;; (set-cursor-type 'box)            ; Meadow-1.10互換 (SKK等で色が変る設定)
;; ;; (set-cursor-type 'hairline-caret) ; 縦棒キャレット


;;; マウスカーソルを消す設定
(setq w32-hide-mouse-on-key t)
(setq w32-hide-mouse-timeout 5000)


;;; font-lockの設定
(global-font-lock-mode t)


;; ;;; TrueType フォント設定
;; (w32-add-font
;;  "private-fontset"
;;  '((spec
;;     ((:char-spec ascii :height 120)
;;      strict
;;      (w32-logfont "Courier New" 0 -13 400 0 nil nil nil 0 1 3 49))
;;     ((:char-spec ascii :height 120 :weight bold)
;;      strict
;;      (w32-logfont "Courier New" 0 -13 700 0 nil nil nil 0 1 3 49))
;;     ((:char-spec ascii :height 120 :slant italic)
;;      strict
;;      (w32-logfont "Courier New" 0 -13 400 0   t nil nil 0 1 3 49))
;;     ((:char-spec ascii :height 120 :weight bold :slant italic)
;;      strict
;;      (w32-logfont "Courier New" 0 -13 700 0   t nil nil 0 1 3 49))
;;     ((:char-spec japanese-jisx0208 :height 120)
;;      strict
;;      (w32-logfont "ＭＳ ゴシック" 0 -16 400 0 nil nil nil 128 1 3 49))
;;     ((:char-spec japanese-jisx0208 :height 120 :weight bold)
;;      strict
;;      (w32-logfont "ＭＳ ゴシック" 0 -16 700 0 nil nil nil 128 1 3 49)
;;      ((spacing . -1)))
;;     ((:char-spec japanese-jisx0208 :height 120 :slant italic)
;;      strict
;;      (w32-logfont "ＭＳ ゴシック" 0 -16 400 0   t nil nil 128 1 3 49))
;;     ((:char-spec japanese-jisx0208 :height 120 :weight bold :slant italic)
;;      strict
;;      (w32-logfont "ＭＳ ゴシック" 0 -16 700 0   t nil nil 128 1 3 49)
;;      ((spacing . -1))))))

;; (set-face-attribute 'variable-pitch nil :family "*")


;; ;;; BDF フォント設定
;;
;; ;;; (方法その1) Netinstall パッケージを使う方法
;; ;;; misc と intlfonts パッケージを入れます。
;; ;;; .emacsの設定
;; (setq bdf-use-intlfonts16 t)
;; (setq initial-frame-alist '((font . "intlfonts16")))
;;
;; ;;; (方法その1') 
;; ;;; intlfonts-file-16dot-alist の形式で bdf-fontset-alist を書き、
;; ;;; 次を設定すれば良い。
;; ;;;  (require 'bdf)
;; ;;;  (bdf-configure-fontset "bdf-fontset" bdf-fontset-alist)
;; ;;; 詳細は $MEADOW/pkginfo/auto-autoloads.el と $MEADOW/site-lisp/bdf.el を
;; ;;; 参照のこと。
;;
;; ;;; (方法その2) 
;; ;;; フォントの指定方法は次のサンプルを参考にする。
;; ;;; normal, bold, italic, bold-itaric フォントを指定する必要あり。
;; (setq bdf-font-directory "c:/Meadow/fonts/intlfonts/")
;; (w32-add-font "bdf-fontset"
;; `((spec 
;;    ;; ascii
;;    ((:char-spec ascii :height any :weight normal :slant normal)
;;     strict (bdf-font ,(expand-file-name "lt1-16-etl.bdf" bdf-font-directory)))
;;    ((:char-spec ascii :height any :weight bold :slant normal)
;;     strict (bdf-font ,(expand-file-name "lt1-16b-etl.bdf" bdf-font-directory)))
;;    ((:char-spec ascii :height any :weight normal :slant any)
;;     strict (bdf-font ,(expand-file-name "lt1-16i-etl.bdf" bdf-font-directory)))
;;    ((:char-spec ascii :height any :weight bold :slant any)
;;     strict (bdf-font ,(expand-file-name "lt1-16bi-etl.bdf" bdf-font-directory)))
;;    ;; katakana-jisx0201
;;    ((:char-spec katakana-jisx0201 :height any :weight normal :slant normal)
;;     strict (bdf-font ,(expand-file-name "8x16rk.bdf" bdf-font-directory))
;;     ((encoding . 1-byte-set-msb))) 
;;    ((:char-spec katakana-jisx0201 :height any :weight bold :slant normal)
;;     strict (bdf-font ,(expand-file-name "8x16rk.bdf" bdf-font-directory))
;;     ((encoding . 1-byte-set-msb))) 
;;    ((:char-spec katakana-jisx0201 :height any :weight normal :slant any)
;;     strict (bdf-font ,(expand-file-name "8x16rk.bdf" bdf-font-directory))
;;     ((encoding . 1-byte-set-msb))) 
;;    ((:char-spec katakana-jisx0201 :height any :weight bold :slant any)
;;     strict (bdf-font ,(expand-file-name "8x16rk.bdf" bdf-font-directory))
;;     ((encoding . 1-byte-set-msb)))
;;    ;; latin-jisx0201
;;    ((:char-spec latin-jisx0201 :height any :weight normal :slant normal)
;;     strict (bdf-font ,(expand-file-name "8x16rk.bdf" bdf-font-directory)))
;;    ((:char-spec latin-jisx0201 :height any :weight bold :slant normal)
;;     strict (bdf-font ,(expand-file-name "8x16rk.bdf" bdf-font-directory)))
;;    ((:char-spec latin-jisx0201 :height any :weight normal :slant any) 
;;     strict (bdf-font ,(expand-file-name "8x16rk.bdf" bdf-font-directory))) 
;;    ((:char-spec latin-jisx0201 :height any :weight bold :slant any) 
;;     strict (bdf-font ,(expand-file-name "8x16rk.bdf" bdf-font-directory)))
;;    ;; japanese-jisx0208
;;    ((:char-spec japanese-jisx0208 :height any :weight normal :slant normal) 
;;     strict (bdf-font ,(expand-file-name "j90-16.bdf" bdf-font-directory)))
;;    ((:char-spec japanese-jisx0208 :height any :weight bold :slant normal)
;;     strict (bdf-font ,(expand-file-name "j90-16.bdf" bdf-font-directory))) 
;;    ((:char-spec japanese-jisx0208 :height any :weight normal :slant any)
;;     strict (bdf-font ,(expand-file-name "j90-16.bdf" bdf-font-directory)))
;;    ((:char-spec japanese-jisx0208 :height any :weight bold :slant any)
;;     strict (bdf-font ,(expand-file-name "j90-16b.bdf" bdf-font-directory))))))

;; 初期フレームの設定
(setq default-frame-alist
      (append (list '(foreground-color . "black")
		    '(background-color . "LemonChiffon")
		    '(background-color . "gray")
		    '(border-color . "black")
		    '(mouse-color . "white")
		    '(cursor-color . "black")
;;		    '(ime-font . (w32-logfont "ＭＳ ゴシック"
;;					      0 16 400 0 nil nil nil
;;					      128 1 3 49)) ; TrueType のみ
;;		    '(font . "bdf-fontset")    ; BDF
;;		    '(font . "private-fontset"); TrueType
		    '(width . 207)
		    '(height . 60)
		    '(top . 10)
		    '(left . 2))
	      default-frame-alist))


;; ;;; shell の設定

;; ;;; Cygwin の bash を使う場合
;; (setq explicit-shell-file-name "bash")
;; (setq shell-file-name "sh")
;; (setq shell-command-switch "-c") 

;; ;;; Virtually UN*X!にある tcsh.exe を使う場合
;; (setq explicit-shell-file-name "tcsh.exe") 
;; (setq shell-file-name "tcsh.exe") 
;; (setq shell-command-switch "-c") 

;; ;;; WindowsNT に付属の CMD.EXE を使う場合。
;; (setq explicit-shell-file-name "CMD.EXE") 
;; (setq shell-file-name "CMD.EXE") 
;; (setq shell-command-switch "\\/c") 

;(setq explicit-shell-file-name "/cygwin/bin/bash.exe")
;(setq shell-file-name "/cygwin/bin/bash.exe")
;(setq shell-command-switch "-c") 


;;; argument-editing の設定
;(require 'mw32script)
;(mw32script-init)


;; ;;; browse-url の設定
;; (global-set-key [S-mouse-2] 'browse-url-at-mouse)

;; c-mode
;(add-hook 'c-mode-hook
;  '(lambda ()
;     (setq c-tab-always-indent t)
;     (setq default-tab-width 4)
;     (setq tab-width 4)
;   )
;)

; c-mode, d-mode 共通
(defun my-c-mode-hook ()
  (c-set-style "linux")
  (setq tab-width 4)
  (setq c-basic-offset tab-width))

; c-mode
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; d-mode
(autoload 'd-mode "d-mode" 
  "Major mode for editing D code." t)
(setq auto-mode-alist (cons '( "\\.d\\'" . d-mode ) auto-mode-alist ))
;(autoload 'dlint-minor-mode "dlint" nil t)
;(add-hook 'd-mode-hook (lambda () (dlint-minor-mode 1)))

(add-hook 'd-mode-hook 'my-c-mode-hook)


;; ;;; 印刷の設定
;; ;; この設定で M-x print-buffer RET などでの印刷ができるようになります
;; ;;
;; ;;  notepad に与えるパラメータの形式の設定
;; (define-process-argument-editing "notepad"
;;   (lambda (x) (general-process-argument-editing-function x nil t)))
;;
;; (defun w32-print-region (start end
;; 				  &optional lpr-prog delete-text buf display
;; 				  &rest rest)
;;   (interactive)
;;   (let ((tmpfile (convert-standard-filename (buffer-name)))
;; 	   (w32-start-process-show-window t)
;; 	   ;; もし、dos 窓が見えていやな人は上記の `t' を `nil' にします
;; 	   ;; ただし、`nil' にすると Meadow が固まる環境もあるかもしれません
;; 	   (coding-system-for-write w32-system-coding-system))
;;     (while (string-match "[/\\]" tmpfile)
;; 	 (setq tmpfile (replace-match "_" t nil tmpfile)))
;;     (setq tmpfile (expand-file-name (concat "_" tmpfile "_")
;; 				       temporary-file-directory))
;;     (write-region start end tmpfile nil 'nomsg)
;;     (call-process "notepad" nil nil nil "/p" tmpfile)
;;     (and (file-readable-p tmpfile) (file-writable-p tmpfile)
;; 	    (delete-file tmpfile))))
;; 
;; (setq print-region-function 'w32-print-region)

;; ;;; fakecygpty の設定
;; ;; この設定で cygwin の仮想端末を要求するプログラムを Meadow から
;; ;; 扱えるようになります
;; (setq mw32-process-wrapper-alist
;;       '(("/\\(bash\\|tcsh\\|svn\\|ssh\\|gpg[esvk]?\\)\\.exe" .
;; 	  (nil . ("fakecygpty.exe" . set-process-connection-type-pty)))))

;; ツールバーを消す
(tool-bar-mode nil)

;; TABはスペース４個分
(setq-default tab-width 4)
(setq-default default-tab-width 4)

;; 長い行の折り返しを t:しない nil:する
(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)


;; buffer list 表示後カーソルをそこに移動する
(define-key ctl-x-map "\C-b" 'buffer-menu)

;; ↓を押しても改行しないようにする
(setq next-line-add-newlines nil)

;; 常に括弧の対応をハイライトする
(show-paren-mode t)

;; 長いパス名のファイルオープンでバッファリサイズ
;;(resize-minibuffer-mode t)

;; 自動水平スクロール
;;(hscroll-global-mode t)
;;(hscroll-mode t)

;; カーソル行表示
(line-number-mode t)

;; カーソル桁数表示
(column-number-mode t)

;; C-kで改行もカット
(setq kill-whole-line t)

;; 出力が到着するたびに出力に追従するようにコンパイルバッファを常にスクロール
(setq compilation-scroll-output t)

;; ワードコピー
(defun y-copy-word ()
  (interactive)
  (command-execute 'backward-word)
  (command-execute 'set-mark-command)
  (command-execute 'forward-word)
  (command-execute 'kill-ring-save)
)

(global-set-key "\C-c\C-w" 'y-copy-word)

;; カスタマイズコンパイル
(defun yrecompile()
  (interactive)
  (if(and
	  (not(string=(buffer-name) "*compilation*"))
	  (get-buffer "*compilation*"))
	  (progn
		(switch-to-buffer-other-window "*compilation*")
		(command-execute 'compile)
		(command-execute 'other-window)
		)
	(progn
	  (command-execute 'compile)
	  (end-of-buffer-other-window "*compilation*")
	  )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインド
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-hをバックスペースに変更
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)

(global-set-key [f2] 'buffer-menu)
(global-set-key [f8] 'goto-line)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

;; ウィンドウ移動
(global-set-key [f4] 'other-window)
(global-set-key [S-f4] 'previous-multiframe-window)

;;表示領域拡大
(global-set-key [C-up] '(lambda() (interactive)(enlarge-window -1)) )
(global-set-key [C-down] '(lambda() (interactive)(enlarge-window +1)) )
(global-set-key [C-left] '(lambda() (interactive)(enlarge-window-horizontally -1)))
(global-set-key [C-right] '(lambda() (interactive)(enlarge-window-horizontally +1)))

;; エラージャンプ
(global-set-key [C-f10] 'next-error)
(global-set-key [C-f11] 'previous-error)

;; 対応する括弧にジャンプ
(global-set-key "\M-m" 'blink-matching-open)

;; 小文字化カスタマイズ（直前のワードを対象にする）
(global-unset-key "\M-l")
(global-set-key "\M-l" (lambda() (interactive)(downcase-word -1)))
;; 大文字化カスタマイズ（直前のワードを対象にする）
(global-unset-key "\M-u")
(global-set-key "\M-u" (lambda() (interactive)(upcase-word -1)))

;; キーボードマクロ
(global-set-key [f9] 'start-kbd-macro)
(global-set-key [f10] 'end-kbd-macro)
(global-set-key [f12] 'call-last-kbd-macro)

;; ファイルの再読み込み
(global-set-key [C-S-f3] 'revert-buffer)

;; コンパイル
(global-set-key [C-f9] 'yrecompile)
(global-set-key [C-M-f9] 'compile)

;;;
;;; end of file
;;;
