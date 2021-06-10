(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq kill-whole-line t)
(setq-default show-trailing-whitespace t)
(setq-default cursor-type 'bar)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)
(global-font-lock-mode 0)
(column-number-mode t)
(show-paren-mode 1)
;; auto-indend without realign current line
(electric-indent-mode 0)
(global-set-key (kbd "RET") #'newline-and-indent)
(load-theme 'acme 1)

(add-hook 'window-setup-hook (lambda ()
                               (tool-bar-mode 0)))

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))

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

(global-set-key (kbd "C-c C-u")
                (lambda
                  (&optional
                   start
                   end)
                  (interactive "r")
                  (pipe-shell-region "uni" start end)))

(defvar formatter "sed 's/[[:blank:]]*$//'")

(global-set-key (kbd "C-x M-f")
                (lambda ()
                  (interactive)
                  (defconst p (point))
                  (pipe-shell-region formatter (point-min)
                                     (point-max))
                  (goto-char p)))

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

;; lsp-mode tweaks https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)
;; lsp-mode settings https://emacs-lsp.github.io/lsp-mode/page/settings/mode/
(setq lsp-keymap-prefix "C-l")
(setq lsp-lens-enable t)
(setq lsp-diagnostic-clean-after-change t)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-haskell-formatting-provider "ormolu")
;; https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
(with-eval-after-load 'lsp-mode (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]deps\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.rebar3?\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cargo\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\MAlonzo\\'"))
(require 'lsp-mode)
