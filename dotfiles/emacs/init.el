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
(show-paren-mode 1)

(when window-system
  (load-theme 'adwaita))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

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

(global-set-key (kbd "C-c C-u") (lambda (&optional start end)
                                  (interactive "r")
                                  (pipe-shell-region "uni" start end)))

(defvar formatter "sed 's/[[:blank:]]*$//'")

(global-set-key (kbd "C-x M-f") (lambda ()
                                  (interactive)
                                  (defconst p (point))
                                  (pipe-shell-region formatter (point-min) (point-max))
                                  (goto-char p)))

(defun haskell-setup ()
  (setq formatter "ormolu"))
(add-hook 'haskell-mode-hook 'haskell-setup)

(defun cabal-setup ()
  (setq formatter "cabal-fmt"))
(add-hook 'cabal-mode-hook 'cabal-setup)

(defun elixir-setup ()
  (setq formatter "mix format"))
(add-hook 'elixir-mode-hook 'elixir-setup)

(defun rust-setup ()
  (setq formatter "rustfmt"))
(add-hook 'rust-mode-hook 'rust-setup)

(setq lsp-haskell-formatting-provider "ormolu")
(setq lsp-keymap-prefix "C-l")
(require 'lsp-mode)
