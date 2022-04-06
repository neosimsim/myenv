(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq kill-whole-line t)
(setq-default cursor-type 'bar)
(setq set-mark-command-repeat-pop t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(setq vc-handled-backends ())
(global-font-lock-mode t)
(column-number-mode t)
(show-paren-mode 1)
;; auto-indent without realign current line
(electric-indent-mode 0)
;; rebind RET because electric-indent-mode is disabled
(global-set-key (kbd "RET") #'newline-and-indent)

(set-fontset-font t '(#x1f000 . #x1faff)
  (font-spec :family "Noto Color Emoji"))

(ivy-mode t)
(counsel-mode t)
(amx-mode)
(setq
   ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   ivy-format-functions-alist
     '((counsel-compile-env . counsel-compile-env--format-hint)
       (counsel-kmacro . counsel--kmacro-format-function)
       (counsel-colors-web . counsel--colors-web-format-function)
       (counsel-colors-emacs . counsel--colors-emacs-format-function)
       (counsel-evil-registers . counsel--yank-pop-format-function)
       (counsel-yank-pop . counsel--yank-pop-format-function)
       (counsel-git-log . counsel--git-log-format-function)
       (counsel-faces . counsel--faces-format-function)
       (swiper-isearch . swiper-isearch-format-function)
       (swiper-all . swiper--all-format-function)
       (swiper-multi . swiper--all-format-function)
       (t . ivy-format-function-arrow)))

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c k") 'counsel-rg)

;; ibuffer is a bit smarter than buffer-menu, e.g. has filters
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-*") 'highlight-symbol-next)
(global-set-key (kbd "C-#") 'highlight-symbol-prev)

(setq
  spacemacs-theme-org-bold nil
  spacemacs-theme-org-height nil)

(defun only-theme (theme)
  (dolist (theme (custom-available-themes))
    (disable-theme theme))
    (load-theme theme 'no-confirm))

(defun light-theme ()
  (interactive)
  (only-theme 'spacemacs-light))

(defun dark-theme ()
  (interactive)
  (only-theme 'spacemacs-dark))

(light-theme)

;; https://emacs.stackexchange.com/questions/21116/how-to-prevent-emacs-from-showing-passphrase-in-m-x-shell
(require 'comint)
;; hide doas prompt
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|password: \\'"))

(defun open ()
  "Open file at point using `open`.

When called from Emacs shell mode this might not work properly since
tracking the current working directory of the shell is sometimes off.
https://www.emacswiki.org/emacs/ShellMode#h5o-9
However, this can be used to (cd) to the proper directory when working
with multi package cabal projects or mix umbrella projects."
  (interactive)
  (start-process "open" nil "open" (thing-at-point 'filename 'no-properties)))

(global-set-key [C-M-mouse-3] 'open)

(defun pipe-shell-region (cmd start end)
  "Pipe region to shell command and replace region with the output
(combined stdout and stderr). The region is only replaced when the
shell command exits 0, otherwise the commands output (combined
stdout and stderr) is displayed in *Shell Command Output*."
  (setq exit-status (call-shell-region start end cmd nil "*Shell Command Output*"))
  (if (equal 0 exit-status)
      (progn (delete-region start end)
             (insert-buffer-substring "*Shell Command Output*")
             (kill-buffer "*Shell Command Output*"))
    (display-buffer "*Shell Command Output*")))

(defun x (regex fn)
  "Like plan9 sam x

Loops over each mach on regex, set point to start of the
match and applies start and end of the match to fn.

Examples:
(x \"emacs\"
  (lambda (start end)
    (call-shell-region start end \"tr [e] [E]\" t (current-buffer))))

(x \"macs\"
  (lambda (start end)
    (insert-char ?E)))

(x \"vim\" #'delete-region)
"
  (let ((pos (if mark-active
                 (min (point) (mark))
                 (point-min)))
        (region-end (if mark-active
                 (max (point) (mark))
                 (point-max))))
    (while (> region-end (string-match regex (buffer-string) pos))
      (setq pos (match-end 0))
      (goto-char (+ 1 (match-beginning 0)))
      (funcall fn (point) (+ 1 pos)))))

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
(defalias 'tm #'send-to-tmux-region)

(defun apply-uni-region
    (&optional
     start
     end)
  "Apply shell command uni on region."
  (interactive "r")
  (pipe-shell-region "uni" start end))
(global-set-key (kbd "C-c C-u") #'apply-uni-region)

(defun my-prog-mode-hook ()
  (setq indicate-empty-lines t)
  (setq show-trailing-whitespace t)
  (display-line-numbers-mode t))
(add-hook 'prog-mode-hook #'my-prog-mode-hook)

(defun my-text-mode-hook ()
  (mixed-pitch-mode t)
  (setq indicate-empty-lines t)
  (setq show-trailing-whitespace t))
(add-hook 'text-mode-hook #'my-text-mode-hook)

(defvar formatter "sed 's/[[:blank:]]*$//'"
  "Commands used by format-buffer")
(make-variable-buffer-local 'formatter)
(defun format-buffer ()
  "Format the current buffer using the shell command stored in formatter."
  (interactive)
  (let ((p (point))
        (prev-point-max (point-max)))
    (pipe-shell-region formatter (point-min) (point-max))
    (goto-char (+ p (- (point-max) prev-point-max)))))
(global-set-key (kbd "C-x M-f") #'format-buffer)

(defun haskell-setup ()
  (setq formatter "ormolu"))
(add-hook 'haskell-mode-hook #'haskell-setup)

(defun cabal-setup ()
  (setq formatter "cabal-fmt"))
(add-hook 'haskell-cabal-mode-hook #'cabal-setup)

(defun elixir-setup ()
  (local-set-key (kbd "C-x M-f") #'elixir-format))
(add-hook 'elixir-mode-hook #'elixir-setup)

(defun copy-buffer-name ()
  (interactive)
  (kill-new (if (buffer-file-name)
                (buffer-file-name)
                (buffer-name))))

(defun rust-setup ()
  (setq formatter "rustfmt"))
(add-hook 'rust-mode-hook #'rust-setup)

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(defun nix-setup ()
  (setq formatter "nixpkgs-fmt"))
(add-hook 'nix-mode-hook #'nix-setup)

(defun my-Info-mode-hook ()
  (mixed-pitch-mode t))
(add-hook 'Info-mode-hook #'my-Info-mode-hook)

(setq org-html-checkbox-type 'unicode)
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (shell . t)))

(setq magit-diff-refine-hunk 'all)

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
(setq lsp-modeline-code-actions-segments '(icon count name))
;; lsp-mode tweaks https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)
;; lsp-mode settings https://emacs-lsp.github.io/lsp-mode/page/settings/mode/
(setq lsp-lens-enable t)
(setq lsp-haskell-formatting-provider "ormolu")
(setq lsp-elixir-dialyzer-enabled nil)
(setq lsp-elixir-server-command '("elixir-ls"))
;; https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
(with-eval-after-load 'lsp-mode (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]deps\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.rebar3?\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cargo\\'")
                      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\MAlonzo\\'"))

;; explicitly require lsp-ui to address the face
(require 'lsp-ui)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-sideline-show-code-actions t)

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))
(defun agda-setup ()
  (setq agda2-highlight-level 'none))
(add-hook 'agda2-mode-hook 'agda-setup)
