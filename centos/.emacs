;; debug mode
(setq debug-on-error t)

;;; 日本語環境設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8) ;; デフォルト
(modify-coding-system-alist 'file "\\.rb\\'" 'utf-8)                ;; Ruby
(modify-coding-system-alist 'file "\\.erb\\'" 'utf-8)               ;; html.erb
(modify-coding-system-alist 'file "\\.html?\\'" 'utf-8)             ;; html, htm
(modify-coding-system-alist 'file "\\.js\\'" 'utf-8)                ;; Java Script
(modify-coding-system-alist 'file "\\.less\\'" 'utf-8)              ;; Less
(modify-coding-system-alist 'file "\\.scss\\'" 'utf-8)              ;; SASS
(modify-coding-system-alist 'file "\\.coffee\\'" 'utf-8)            ;; Coffee Script
(modify-coding-system-alist 'file "\\.java\\'" 'utf-8)              ;; Java
(modify-coding-system-alist 'file "\\.clj\\'" 'utf-8)               ;; Clojure
(modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\)\\'" 'utf-8) ;; Scala
(modify-coding-system-alist 'file "\\.[eh]rl\\'" 'utf-8)            ;; Erlang
(modify-coding-system-alist 'file "\\.exs?\\'" 'utf-8)              ;; Elixir

;; ミニバッファに移動した際は最初に日本語入力が無効な状態にする
(add-hook 'minibuffer-setup-hook 'deactivate-input-method)

;; isearch に移行した際に日本語入力を無効にする
(add-hook 'isearch-mode-hook '(lambda ()
                                (deactivate-input-method)
                                (setq w32-ime-composition-window (minibuffer-window))))
(add-hook 'isearch-mode-end-hook '(lambda () (setq w32-ime-composition-window nil)))

;;; マウスカーソルを消す設定
(setq w32-hide-mouse-on-key t)
(setq w32-hide-mouse-timeout 5000)

;; ビープ音を消す
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; TABはスペース４個分
(setq-default tab-width 4)
(setq-default default-tab-width 4)

;;; 初期フレームの設定
(setq default-frame-alist
      (append (list
			         '(foreground-color . "black")
			         '(background-color . "LemonChiffon")
			         ;;            '(background-color . "gray")
			         '(border-color . "black")
			         '(mouse-color . "white")
			         '(cursor-color . "black")
			         ;;            '(font . "-*-Menlo-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
			         '(width . 190)
			         '(height . 51)
			         '(top . 0)
			         '(left . 0)
			         )
              default-frame-alist))

;;;; ロードパス
(add-to-list 'load-path "~/emacs/lisp/")
;; PATH
(dolist (dir (list
	            "/sbin"
	            "/usr/sbin"
	            "/bin"
	            "/usr/bin"
	            "/usr/local/bin"
	            (expand-file-name "~/bin")
	            ))
  ;; exec-path
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;;; デフォルトディレクトリ
(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; melpaパッケージ
(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))


;;; minibuffer
(define-key minibuffer-local-map (kbd "C-p") 'previous-line-or-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-line-or-history-element)

;; 色の設定
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "#D9333F"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
 '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face ((t (:foreground "#E6B422" :weight bold))))
 '(web-mode-server-comment-face ((t (:foreground "#D9333F"))))
 '(web-mode-symbol-face ((t (:foreground "blue")))))
; ハイライト色の設定
(eval-after-load "web-mode"
  '(set-face-background 'web-mode-current-element-highlight-face "blue"))

;; ファイルとモードの関連付け
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss$"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$"        . web-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.rb$"        . ruby-mode))

;; 長い行の折り返しを t:しない nil:する
(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)

;; buffer list 表示後カーソルをそこに移動する
(define-key ctl-x-map "\C-b" 'buffer-menu)

;; ↓を押しても改行しないようにする
(setq next-line-add-newlines nil)

;; 常に括弧の対応をハイライトする
(show-paren-mode t)

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
(defun kill-ring-save-current-word ()
"Save current word to kill ring as if killed, but don't kill it."
(interactive)
(kill-new (current-word)))
(global-set-key "\C-c\C-w" 'kill-ring-save-current-word)

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

;; コメント
;; 複数業コメント
(defun insert-comment-function ()
  (interactive)
  (insert "/**
*
*/"))

;; １行コメント
(defun insert-line-comment-function ()
  (interactive)
  (insert "///< @todo ")
  )

;; savehist
(savehist-mode 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c-mode, d-mode 共通
(defun my-c-mode-common-init ()
  (c-set-style "linux")
  (setq tab-width 4)
  (setq c-basic-offset tab-width)
  ;;(c-toggle-auto-hungry-state 1)  ;; センテンスの終了である ';' を入力したら、自動改行+インデント
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)  ;; RET キーで自動改行+インデント
  (local-unset-key "\C-c\C-w") ; subword-mode切り替えを無効化
  (gtags-mode 1)
  (add-to-list 'ac-sources 'ac-source-gtags)
  )

(add-hook 'c-mode-hook 'my-c-mode-on-init)
(add-hook 'c++-mode-hook 'my-c-mode-common-init)

;; d-mode
(autoload 'd-mode "d-mode"
  "Major mode for editing D code." t)
(setq auto-mode-alist (cons '( "\\.d\\'" . d-mode ) auto-mode-alist ))
                                        ;(autoload 'dlint-minor-mode "dlint" nil t)
                                        ;(add-hook 'd-mode-hook (lambda () (dlint-minor-mode 1)))

(add-hook 'd-mode-hook 'my-c-mode-hook)

;; .ino をc-modeで開く
(setq auto-mode-alist
      (append '(("\\.ino$" . c++-mode))
              auto-mode-alist))

;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (modify-syntax-entry ?- "w") 
                                   (modify-syntax-entry ?_ "w")
                                   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; キーバインド
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 無効化
(define-key emacs-lisp-mode-map (kbd "C-M-x") nil)
(global-set-key (kbd "C-z") nil)

;; macバックスラッシュキー対策
(define-key global-map [?\¥] [?\\])
(define-key global-map [?\C-¥] [?\C-\\])
(define-key global-map [?\M-¥] [?\M-\\])
(define-key global-map [?\C-\M-¥] [?\C-\M-\\])

;; C-hをバックスペースに変更
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)

;;(global-set-key [f2] 'buffer-menu)
(global-set-key [f2] 'helm-buffers-list)
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

;; 最近使ったファイル
(require 'recentf)
(setq recentf-max-saved-items 50)            ;; recentf に保存するファイルの数
(recentf-mode 1)
(global-set-key [f7] 'recentf-open-files)

;; コメント
(global-set-key "\M-]" 'insert-comment-function)
(global-set-key "\M-[" 'insert-line-comment-function)

;; フォントサイズ変更キー
; C-+ で拡大
(global-set-key [(control ?\;)] (lambda () (interactive) (text-scale-increase 1)))
; C-- で縮小
(global-set-key [(control ?-)] (lambda () (interactive) (text-scale-decrease 1)))
; C-0 でデフォルトに戻す
(global-set-key [(control ?0)] (lambda () (interactive) (text-scale-increase 0)))


;;;
;;; end of file
;;;
