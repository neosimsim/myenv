(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

(use-package emacs
  :custom
  (ring-bell-function #'ignore)
  (create-lockfiles nil)
  (cursor-type 'bar)
  (frame-resize-pixelwise t "Don't leave empty boarders when maximixed on e.g. KDE")
  (default-frame-alist '((fullscreen . maximized)) "Start in fullscreen")

  :hook
  ('before-save . #'delete-trailing-whitespace)

  :config
  (show-paren-mode 1)

  (menu-bar-mode 0)

  ;; GUI specifics:
  ;; Note that (display-graphic-p) does not work in case of emacs-server
  (when (fboundp #'tool-bar-mode)
    (tool-bar-mode 0))
  (when (fboundp #'scroll-bar-mode)
    (scroll-bar-mode 0))

  (when (fboundp #'set-fontset-font)
    (set-fontset-font t '(#x1f000 . #x1faff)
                      (font-spec :family "Noto Color Emoji"))))

(use-package files
  :custom
  (auto-save-default nil)
  (make-backup-files nil))

(use-package simple
  :custom
  (kill-whole-line t "Also delete new-line")
  (set-mark-command-repeat-pop t)
  (column-number-mode t "Also show column of point in info bar"))

(use-package ivy
  :config
  (ivy-mode t)

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-format-functions-alist
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

  :bind (("C-c C-r" . #'ivy-resume)))

(use-package counsel
  :config
  (counsel-mode t)

  :custom
  (counsel-rg-base-command '("rg" "--max-columns" "240" "--with-filename" "--no-heading"
			     "--line-number" "--color" "never" "%s" "--hidden" "--glob" "!.git"))

  :bind (("C-c g" . #'counsel-git)
         ("C-c k" . #'counsel-rg)))

(use-package amx
  :config
  (amx-mode 1))

;; ibuffer is a bit smarter than buffer-menu, e.g. has filters
(use-package ibuffer
  :bind ("C-x C-b" . #'ibuffer))

(use-package avy
  :bind ("C-:" . #'ay-goto-char))

(use-package highlight-symbol
  :bind (("C-*" . #'highlight-symbol-next)
	 ("C-#" . #'highlight-symbol-prev)))

(use-package browse-url
  :custom
  (browse-url-browser-function #'browse-url-chromium))

(use-package ediff
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package spacemacs-theme
  :custom
  (spacemacs-theme-org-bold nil)
  (spacemacs-theme-org-height nil))

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

;; https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
(defcustom project-root-markers
  '("Cargo.lock" "mix.lock" ".git")
  "File or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun project-root-p (path)
   ""
   (catch 'found
     (dolist  (marker project-root-markers)
       (when (file-exists-p (concat path marker))
         (throw 'found marker)))))

(defun project-find-root (path)
   "Serach up for project root"
   (when-let ((root (locate-dominating-file path #'project-root-p)))
     (cons 'transient (expand-file-name root))))

(add-to-list 'project-find-functions #'project-find-root)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls"))))

(use-package comint
  :custom
  ;; https://emacs.stackexchange.com/questions/21116/how-to-prevent-emacs-from-showing-passphrase-in-m-x-shell
  (comint-password-prompt-regexp (concat comint-password-prompt-regexp "\\|password: \\'") "hide doas prompt"))

(defun dtt ()
  "Run `dtt` with file at point.

When called from Emacs shell mode this might not work properly since
tracking the current working directory of the shell is sometimes off.
https://www.emacswiki.org/emacs/ShellMode#h5o-9
However, this can be used to (cd) to the proper directory when working
with multi package cabal projects or mix umbrella projects."
  (interactive)
  (start-process "dtt" nil "dtt" (thing-at-point 'symbol 'no-properties)))

(global-set-key [C-M-mouse-3] 'dtt)

(defun pipe-shell-region (cmd start end)
  "Pipe region to shell command and replace region with the output
(combined stdout and stderr). The region is only replaced when the
shell command exits 0, otherwise the commands output (combined
stdout and stderr) is displayed in *Shell Command Output*."
  (let ((buffer (get-buffer-create (format "*Pipe Shell Region Output: %s*" cmd))))
    (with-current-buffer buffer (delete-region (point-min) (point-max)))
    (if (equal 0 (call-shell-region start end cmd nil buffer))
      (progn (unless (string=
                      (buffer-substring-no-properties start end)
                      (with-current-buffer buffer (buffer-string)))
               (delete-region start end)
               (insert-buffer-substring buffer))
             (kill-buffer buffer))
      (display-buffer buffer))))

(defun track ()
  (interactive)
  (find-file (substring (shell-command-to-string "date +'~/doc/tracking/%Y-%m'") 0 -1)))

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

(defun tmux-git ()
  (interactive)
  (let ((git-root (substring (shell-command-to-string "git rev-parse --show-toplevel") 0 -1)))
    (call-process "tmux" nil nil nil "new-window" "-c" git-root)))

(defun tmux-cwd ()
  (interactive)
  (call-process "tmux" nil nil nil "new-window" "-c" default-directory))

(defun apply-uni-region
    (&optional
     start
     end)
  "Apply shell command uni on region."
  (interactive "r")
  (pipe-shell-region "uni" start end))
(keymap-global-set "C-c C-u" #'apply-uni-region)

(defun copy-buffer-name ()
  (interactive)
  (kill-new (if (buffer-file-name)
                (buffer-file-name)
                (buffer-name))))

(use-package move-text
  :config
  (move-text-default-bindings))

(defun my-prog-mode-hook ()
  (setq indicate-empty-lines t)
  (setq show-trailing-whitespace t)
  (display-line-numbers-mode t))
(add-hook 'prog-mode-hook #'my-prog-mode-hook)

(defun my-text-mode-hook ()
  (mixed-pitch-mode t)
  (setq indicate-empty-lines t)
  (setq show-trailing-whitespace t)
  (setq truncate-lines nil)) ;; Force line wrapping. I prefer this over visual-line-mode
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
(keymap-global-set "C-x M-f" #'format-buffer)

(defun haskell-setup ()
  (setq formatter "ormolu --no-cabal"))
(add-hook 'haskell-mode-hook #'haskell-setup)

(defun cabal-setup ()
  (setq formatter "cabal-fmt"))
(add-hook 'haskell-cabal-mode-hook #'cabal-setup)

(defun fish-setup ()
  (local-set-key (kbd "C-x M-f") #'fish_indent))
(add-hook 'fish-mode-hook #'fish-setup)

(defun elixir-setup ()
  (local-set-key (kbd "C-x M-f") #'elixir-format)
  (modify-syntax-entry ?& "." elixir-mode-syntax-table))
(add-hook 'elixir-mode-hook #'elixir-setup)

(defun term-setup ()
  (modify-syntax-entry ?: "_" term-mode-syntax-table)
  (modify-syntax-entry ?. "_" term-mode-syntax-table))
(add-hook 'term-mode-hook #'term-setup)
(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")

(defun python-setup ()
  (elpy-enable)
  (local-set-key (kbd "C-x M-f") #'elpy-format-code))
(add-hook 'python-mode-hook #'python-setup)

(defun rust-setup ()
  (setq formatter "rustfmt"))
(add-hook 'rust-mode-hook #'rust-setup)

(use-package rustic
  :custom
  (rustic-lsp-client #'eglot))

(use-package nix-mode
  :mode "\\.nix\\'")

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

(use-package buffer-move
  :bind (("C-S-<up>"    . 'buf-move-up)
	 ("C-S-<down>"  . 'buf-move-down)
	 ("C-S-<left>"  . 'buf-move-left)
	 ("C-S-<right>" . 'buf-move-right)))

(use-package elisp-format)
