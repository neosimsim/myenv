(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq kill-whole-line t)
(setq-default show-trailing-whitespace t)
(setq-default cursor-type 'bar)
(setq set-mark-command-repeat-pop t)
(tool-bar-mode 0)
(global-display-line-numbers-mode t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq vc-handled-backends ())
(global-font-lock-mode 0)
(column-number-mode t)
(show-paren-mode 1)
;; auto-indend without realign current line
(electric-indent-mode 0)
(global-set-key (kbd "RET") #'newline-and-indent)
(load-theme 'acme 1)
(set-face-attribute 'isearch-fail nil
                    :foreground "white smoke"
                    :weight 'bold)

(global-set-key [M-right] 'forward-whitespace)
(defun backward-whitespace ()
  "Move point to the beginneg of the previous sequence of whitespace char"
  (interactive)
  (forward-whitespace -1))
(global-set-key [M-left] 'backward-whitespace)

;; https://emacs.stackexchange.com/questions/21116/how-to-prevent-emacs-from-showing-passphrase-in-m-x-shell
(require 'comint)
;; hide doas prompt
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|password: \\'"))

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))
(defun agda-setup ()
  (setq agda2-highlight-level 'none))
(add-hook 'agda2-mode-hook 'agda-setup)

(defun open ()
  "Open file at point using `open`."
  (interactive)
  (start-process "open" nil "open" (thing-at-point 'filename 'no-properties)))

(defun pipe-shell-region (cmd start end)
  "Pipe region to shell command and replace region with the output
(combined stdout and stderr). The region is only replaced when the
shell command exits 0, otherwise the commands output (combined
stdout and stderr) in displayed in a new buffer."
  (defconst out-buffer (generate-new-buffer "pipe"))
  (setq exit-status (call-shell-region start end cmd nil out-buffer))
  (if (equal 0 exit-status)
      (progn (delete-region start end)
             (insert-buffer-substring out-buffer)
             (kill-buffer out-buffer))
    (display-buffer out-buffer)))

(defun plumb-file ()
  "Sends filename to plumber"
  (interactive)
  (call-process "9" nil nil nil
                "plumb"
                "-d" "edit"
                "-a" (concat "addr=#" (number-to-string (- (point) 1)))
                (buffer-file-name)))

(defun send-to-tmux-region
    (&optional
     start
     end)
  "Sends the range to the current tmux pane followed by Enter"
  (interactive "r")
  (call-process "tmux" nil nil nil "send-keys"
                (buffer-substring-no-properties
                 start
                 end)
                "Enter"))
(defalias 'tm 'send-to-tmux-region)

(defun apply-uni-region
    (&optional
     start
     end)
  "Apply shell command uni on region."
  (interactive "r")
  (pipe-shell-region "uni" start end))
(global-set-key (kbd "C-c C-u") #'apply-uni-region)

(defvar formatter "sed 's/[[:blank:]]*$//'"
  "Commands used by format-buffer")
(make-variable-buffer-local 'formatter)
(defun format-buffer ()
  "Format the current buffer using the shell command stored in formatter."
  (interactive)
  (defconst p (point))
  (pipe-shell-region formatter (point-min)
                     (point-max))
  (goto-char p))
(global-set-key (kbd "C-x M-f") #'format-buffer)

(defun haskell-setup ()
  (setq formatter "ormolu"))
(add-hook 'haskell-mode-hook #'haskell-setup)

(defun cabal-setup ()
  (setq formatter "cabal-fmt"))
(add-hook 'haskell-cabal-mode-hook #'cabal-setup)

(defun elixir-setup ()
  (setq formatter "mix format"))
(add-hook 'elixir-mode-hook #'elixir-setup)

(defun rust-setup ()
  (setq formatter "rustfmt"))
(add-hook 'rust-mode-hook #'rust-setup)

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(defun nix-setup ()
  (setq formatter "nixpkgs-fmt"))
(add-hook 'nix-mode-hook #'nix-setup)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
(global-set-key (kbd "C->") #'mc/mark-next-like-this-word)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-c C-<") #'mc/mark-all-word-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(require 'elisp-format)

;; flycheck will be enabled by lsp-mode
(require 'flycheck)
;; display the error list at the bottom side of the frame
(add-to-list 'display-buffer-alist `(,(rx bos "*Flycheck errors*" eos)
                                     (display-buffer-reuse-window display-buffer-in-side-window)
                                     (side            . bottom)
                                     (reusable-frames . visible)
                                     (window-height   . 0.33)))

(global-set-key (kbd "C-c C-.") 'company-complete)

;; set prefix before lsp-mode has been loaded
;; https://github.com/emacs-lsp/lsp-mode/issues/1672
(setq lsp-keymap-prefix "C-.")
(require 'lsp-mode)
;; lsp-mode tweaks https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)
;; lsp-mode settings https://emacs-lsp.github.io/lsp-mode/page/settings/mode/
(setq lsp-lens-enable t)
(setq lsp-haskell-formatting-provider "ormolu")
(setq lsp-elixir-dialyzer-enabled nil)
;; https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
(with-eval-after-load 'lsp-mode (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]deps\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.rebar3?\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cargo\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\MAlonzo\\'"))

;; explicitly require lsp-ui to address the face
(require 'lsp-ui)
(set-face-foreground 'lsp-ui-sideline-code-action "dim gray")
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-show-with-mouse nil)
