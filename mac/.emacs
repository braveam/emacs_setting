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


;; IME
;;(setq default-input-method "W32-IME")
;;(setq-default w32-ime-mode-line-state-indicator "[--]")
;;(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
;;(w32-ime-initialize)
;; 日本語入力時にカーソルの色を変える設定 (色は適宜変えてください)
(add-hook 'w32-ime-on-hook '(lambda () (set-cursor-color "coral4")))
(add-hook 'w32-ime-off-hook '(lambda () (set-cursor-color "black")))

;; ミニバッファに移動した際は最初に日本語入力が無効な状態にする
(add-hook 'minibuffer-setup-hook 'deactivate-input-method)

;; isearch に移行した際に日本語入力を無効にする
(add-hook 'isearch-mode-hook '(lambda ()
                                (deactivate-input-method)
                                (setq w32-ime-composition-window (minibuffer-window))))
(add-hook 'isearch-mode-end-hook '(lambda () (setq w32-ime-composition-window nil)))

;; helm 使用中に日本語入力を無効にする
(advice-add 'helm :around '(lambda (orig-fun &rest args)
                             (let ((select-window-functions nil)
                                   (w32-ime-composition-window (minibuffer-window)))
                               (deactivate-input-method)
                               (apply orig-fun args))))

;;; 英数モード関連
;;; emacs 起動時は英数モードから始める
;(add-hook 'after-init-hook '(lambda() (interactive)(set-input-method "japanese-ascii")) )
;;; minibuffer 内は英数モードにする
;(add-hook 'minibuffer-setup-hook '(lambda() (interactive)(set-input-method "japanese-ascii")) )
;;; [migemo]isearch のとき IME を英数モードにする
										;(add-hook 'isearch-mode-hook '(lambda() (interactive)(set-input-method "japanese-ascii")) )

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
            '(width . 180)
            '(height . 50)
            '(top . 0)
            '(left . 0)
            )
            default-frame-alist))

;;;; ロードパス
(add-to-list 'load-path "~/emacs/lisp/")

;; 
(setq default-directory "~/") 
(setq command-line-default-directory "~/")

;; melpaパッケージ
(require 'package) ;; You might already have this line
;(add-to-list 'package-archives
;             '("melpa" . "http://melpa.org/packages/"))
;(when (< emacs-major-version 24)
;  ;; For important compatibility libraries like cl-lib
;  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;(package-initialize) ;; You might already have this line
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))


;;; インデントハイライト
;(add-to-list 'load-path "~/emacs/lisp/Highlight-Indentation-for-Emacs")
;(require 'highlight-indentation)
;(setq highlight-indentation-offset 2)
;;(set-face-background 'highlight-indentation-face "ivory1")
;(set-face-background 'highlight-indentation-face "LightYellow1")
;(set-face-background 'highlight-indentation-current-column-face "AntiqueWhite1")
;
;; highlight-indentation-mode が呼ばれたら highlight-indentation-current-column-mode も実行する
;(add-hook 'highlight-indentation-mode-hook 'highlight-indentation-current-column-mode)

; c-mode, d-mode 共通
(defun my-c-mode-common-init ()
  (c-set-style "linux")
  (setq tab-width 4)
  (setq c-basic-offset tab-width)
  ;(c-toggle-auto-hungry-state 1)  ;; センテンスの終了である ';' を入力したら、自動改行+インデント
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

;;; yaml-mode
;(require 'yaml-mode)
;(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;(add-hook 'yaml-mode-hook 'highlight-indentation-mode)

;; web-mode
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq-default tab-width 2 indent-tabs-mode nil)
  (setq web-mode-html-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (highlight-indentation-mode)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
; 色の設定
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
; キーがかぶるので無効化
(define-key web-mode-map (kbd "\C-c\C-w") nil)

;; ruby-mode
(add-hook 'ruby-mode-hook 'highlight-indentation-mode)

;; gnu global
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))

;; ;;; 印刷の設定
;; ;; この設定で M-x print-buffer RET などでの印刷ができるようになります
;; ;;
;; ;;  notepad に与えるパラメータの形式の設定
;; (define-process-argument-editing "notepad"
;;   (lambda (x) (general-process-argument-editing-function x nil t)))
;;
;; (defun w32-print-region (start end
;;                   &optional lpr-prog delete-text buf display
;;                   &rest rest)
;;   (interactive)
;;   (let ((tmpfile (convert-standard-filename (buffer-name)))
;;        (w32-start-process-show-window t)
;;        ;; もし、dos 窓が見えていやな人は上記の `t' を `nil' にします
;;        ;; ただし、`nil' にすると Meadow が固まる環境もあるかもしれません
;;        (coding-system-for-write w32-system-coding-system))
;;     (while (string-match "[/\\]" tmpfile)
;;      (setq tmpfile (replace-match "_" t nil tmpfile)))
;;     (setq tmpfile (expand-file-name (concat "_" tmpfile "_")
;;                        temporary-file-directory))
;;     (write-region start end tmpfile nil 'nomsg)
;;     (call-process "notepad" nil nil nil "/p" tmpfile)
;;     (and (file-readable-p tmpfile) (file-writable-p tmpfile)
;;         (delete-file tmpfile))))
;;
;; (setq print-region-function 'w32-print-region)

;; ;;; fakecygpty の設定
;; ;; この設定で cygwin の仮想端末を要求するプログラムを Meadow から
;; ;; 扱えるようになります
;; (setq mw32-process-wrapper-alist
;;       '(("/\\(bash\\|tcsh\\|svn\\|ssh\\|gpg[esvk]?\\)\\.exe" .
;;       (nil . ("fakecygpty.exe" . set-process-connection-type-pty)))))

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

;;; auto-complete
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;(ac-config-default)
;(global-auto-complete-mode t)
;(setq ac-use-menu-map t) ;; メニュー表示時のみに有効になるキーマップ(ac-menu-map)を利用
;
;(setq ac-use-quick-help t)
;(setq ac-quick-help-delay 0.5)
;(setq ac-menu-height 20)
;(setq ac-auto-show-menu 0.1)    ;; 0.1秒後に自動的に表示
;(ac-set-trigger-key "TAB")
;(add-to-list 'ac-ignores "/")
;(add-to-list 'ac-ignores "//")
;(add-to-list 'ac-ignores "///")
;(add-to-list 'ac-ignores "////")
;(define-key ac-mode-map (kbd "C-/") 'auto-complete)

;;; grep-edit
;(require 'grep-edit)

;;; migemo
(require 'migemo)
;;;for mac
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-coding-system 'utf-8-unix)
;; for Windows
;(setq migemo-command "cmigemo")
;(setq migemo-dictionary "Z:/home/lisp/cmigemo-default-win64/dict/cp932")
;(setq migemo-dictionary "Z:/home/lisp/cmigemo-default-win64/dict/utf-8/migemo-dict")
;(setq migemo-options '("-q" "--emacs" "-i" "\a"))
;(setq migemo-options '("-q" "--emacs"))
;(setq migemo-coding-system 'cp932-unix)
;(setq migemo-coding-system 'utf-8-unix)
;; migemo common
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(load-library "migemo")
(migemo-init)

;; helm
(helm-mode +1)
; ファイル履歴
(global-set-key [f7] 'helm-recentf)

;(define-key helm-map (kbd "<tab>") 'helm-next-source)
(define-key helm-map (kbd "<tab>") 'dabbrev-expand)
(define-key helm-map (kbd "<right>") 'helm-select-action)
; TABで補完
(define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)

;; helm-swoop
(add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")
(require 'helm-swoop)
(global-set-key (kbd "C-S-s") 'helm-swoop)
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-swoop-map (kbd "C-a") 'helm-maybe-exit-minibuffer)

;; savehist
(savehist-mode 1)

;;; projectile-rails
;(require 'projectile)
;(projectile-global-mode)
;
;(require 'projectile-rails)
;(add-hook 'projectile-mode-hook 'projectile-rails-on)
;
;(define-key projectile-rails-mode-map (kbd "C-c ; m") 'projectile-rails-find-model)
;(define-key projectile-rails-mode-map (kbd "C-c ; c") 'projectile-rails-find-controller)
;(define-key projectile-rails-mode-map (kbd "C-c ; v") 'projectile-rails-find-view)
;(define-key projectile-rails-mode-map (kbd "C-c ; f m") 'projectile-rails-find-current-model)
;(define-key projectile-rails-mode-map (kbd "C-c ; f c") 'projectile-rails-find-current-controller)
;(define-key projectile-rails-mode-map (kbd "C-c ; f v") 'projectile-rails-find-current-view)
;(define-key projectile-rails-mode-map (kbd "C-c ; f s") 'projectile-rails-find-current-spec)
;;(define-key projectile-rails-mode-map (kbd "C-c ; c") 'projectile-rails-console)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインド
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macバックスラッシュキー対策
(define-key global-map [?\\] [?\\])
(define-key global-map [?\C-\] [?\C-\\])
(define-key global-map [?\M-\] [?\M-\\])
(define-key global-map [?\C-\M-\] [?\C-\M-\\])

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

;; 最近使ったファイル
(require 'recentf)
(setq recentf-max-saved-items 50)            ;; recentf に保存するファイルの数
(recentf-mode 1)
;;(global-set-key [f7] 'recentf-open-files)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages (quote (multi-web-mode wgrep helm-swoop migemo helm)))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
