;; -*- lexical-binding: t; -*-

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

(use-package emacs
  :custom
  (ring-bell-function #'ignore)
  (create-lockfiles nil)
  (cursor-type 'bar)
  (frame-resize-pixelwise t "Don't leave empty boarders when maximized on e.g. KDE")
  (default-frame-alist '((fullscreen . maximized)) "Start in fullscreen")

  :hook
  ('before-save . delete-trailing-whitespace)

  :config
  (show-paren-mode 1)
  (global-display-line-numbers-mode)

  (menu-bar-mode 0)

  ;; Enforce US time locale to ensure compatibility of Org Mode timestamps with Plain Org.
  (setq system-time-locale "en_US.UTF-8")

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
  (make-backup-files nil)
  (major-mode-remap-alist
   '((elixir-mode . elixir-ts-mode)
     (js-json-mode . json-ts-mode)
     (python-mode . python-ts-mode))))

(use-package simple
  :custom
  (kill-whole-line t "Also delete new-line")
  (set-mark-command-repeat-pop t)
  (column-number-mode t "Also show column of point in info bar"))

(use-package minibuffer
  :custom
  (completion-styles '(basic partial-completion flex emacs22)))

(use-package custom
  :config
  (setq custom-file "~/.config/emacs-custom.el")
  (load custom-file))

(use-package em-term
  :defer t
  :config
  (add-to-list 'eshell-visual-subcommands '("git" . ("log" "diff" "show")))
  (add-to-list 'eshell-visual-options '("git" . ("--help" "--paginate")))

  (add-to-list 'eshell-visual-subcommands '("nix" . ("build" "shell")))
  (add-to-list 'eshell-visual-options '("nix" . ("--help")))

  (add-to-list 'eshell-visual-commands "home-manager"))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-g M-n" . flymake-goto-next-error)
              ("M-g M-p" . flymake-goto-prev-error)))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-j" . org-clock-goto)
         ("C-c C-x C-x" . org-clock-in-last)
         ([remap org-set-tags-command] . counsel-org-tag))

  :custom
  (org-modules '(ol-doi
                 ol-w3m
                 ol-bbdb
                 ol-bibtex
                 ol-docview
                 ol-gnus
                 org-habit
                 ol-info
                 ol-irc
                 ol-mhe
                 ol-rmail
                 ol-eww))

  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n!/!)" "|" "DONE(d!/!)")))

  (org-agenda-start-with-log-mode t)
  (org-log-done t)
  (org-log-into-drawer t)

  (org-agenda-custom-commands '(("d" "Dashboard"
                                 ((agenda "")
                                  (todo "NEXT"
                                        ((org-agenda-overriding-header "Next Tasks")))
                                  (alltodo ""
                                           ((org-agenda-overriding-header "Add TODOs")))))

                                ("n" "Next Tasks" todo "NEXT"
                                 ((org-agenda-overriding-header "Next Tasks")))

                                ("w" "Work TODOs" tags-todo "work"
                                 ((org-agenda-overriding-header "Work TODOs")))))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

(use-package org-capture
  :defer t
  :custom
  (org-capture-templates '(("t" "Task" entry (file "~/notes/todo.org")
                            "" :empty-lines 1)

                           ("o" "Offenen Fragen" entry (clock)
                            "" :empty-lines 1)

                           ("b" "Bookmark link from clipboard" entry (file "~/notes/links.org")
                            "* %(org-cliplink-capture)"
                            :immediate-finish t
                            :jump-to-captured t))))

(use-package ox-html
  :defer t
  :custom
  (org-html-checkbox-type 'unicode))

(defun neosimsim-git-commit-mode-hook ()
  (flyspell-mode))

(use-package git-commit
  :defer t
  :config

  (add-hook 'git-commit-mode-hook #'neosimsim-git-commit-mode-hook))

(use-package magit
  :custom
  (magit-blame-echo-style 'show-lines
                          "Show commit info before chunks. The default value 'lines just shows an empty line"))

;; Ensure magit-extras for `magit-project-status'.
(use-package magit-extras
  :after (:any project magit))

(use-package ivy
  :disabled
  :demand t
  :config
  (ivy-mode t)

  :custom
  (ivy-extra-directories ())
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
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

  :bind (("C-c C-r" . ivy-resume)
         :map ivy-mode-map
         ("C-<return>" . ivy-immediate-done)))

(use-package counsel
  :disabled
  :demand t
  :config
  (counsel-mode t)

  :custom
  (counsel-rg-base-command '("rg" "--max-columns" "240" "--with-filename" "--no-heading"
                             "--line-number" "--color" "never" "%s" "--hidden" "--glob" "!.git"))

  :bind (("C-c g" . counsel-git)
         ("C-c k" . counsel-rg)))

(use-package swiper
  :disabled
  :bind
  ("C-S-s" . swiper))

(use-package amx
  :disabled
  :config
  (amx-mode 1))

;; ibuffer is a bit smarter than buffer-menu, e.g. has filters
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package avy
  :bind ("C-:" . avy-goto-char))

(use-package highlight-symbol
  :bind (("C-*" . highlight-symbol-next)
         ("C-#" . highlight-symbol-prev)))

(use-package ediff
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package spacemacs-theme
  :disabled
  :custom
  (spacemacs-theme-org-bold nil)
  (spacemacs-theme-org-height nil)

  :config
  (defun light-theme ()
    (interactive)
    (load-theme 'spacemacs-light t)
    (disable-theme 'spacemacs-dark))

  (defun dark-theme ()
    (interactive)
    (load-theme 'spacemacs-dark t)
    (disable-theme 'spacemacs-light))

  (light-theme))

(use-package color-theme-sanityinc-tomorrow
  :disabled
  :config
  (color-theme-sanityinc-tomorrow-day))

(use-package leuven-theme
  :config
  (load-theme 'leuven t)

  :custom-face
  (eglot-mode-line ((t (:inherit (mode-line-emphasis))))))

;; Idea from https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
(defcustom neosimsim-project-root-markers
  '(".git")
  "File or directories that indicate the root of a project.

Sometimes a git repo consist of multiple projects, this function looks
for special files and directories marking such project."
  :type '(repeat string)
  :group 'project)

(make-variable-buffer-local 'neosimsim-project-root-markers)

(defun neosimsim--project-root-p (path)
  "Check if PATH is a project root."
  (seq-find (lambda (marker) (file-exists-p (concat path marker))) neosimsim-project-root-markers))

(defun neosimsim-project-find-root (path)
  "Search up from PATH for project root.

This functions takes into account that a sub-folder of a git repo might
be a sub-project root.  In that case the sub-folder is returned as root
but if under version control, it is still marked as such, so that the
correct implementation for `project-files' is used and files ignored by
version controller are excluded."
  (when-let ((root (locate-dominating-file path #'neosimsim--project-root-p)))
    (if (project-try-vc root)
        (list 'vc 'Git (expand-file-name root))
      (cons 'transient (expand-file-name root)))))

(use-package project
  :defer t
  :functions
  project-try-vc

  :config
  (add-to-list 'project-find-functions #'neosimsim-project-find-root))

(defun neosimsim-eglot-managed-mode-hook ()
  (eglot-inlay-hints-mode -1))

(use-package eglot
  :defer t
  :functions eglot-inlay-hints-mode
  :config
  (add-hook 'eglot-managed-mode-hook #'neosimsim-eglot-managed-mode-hook)

  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode) . ("lexical")))

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode rustic-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  :bind (:map eglot-mode-map
              ([remap format-buffer] . eglot-format-buffer)
              ("C-c C-e a" . eglot-code-actions)
              ("C-c C-e i" . eglot-code-action-organize-imports)
              ("C-c C-e r" . eglot-rename)))

(use-package eglot-x
  :functions
  eglot-x-setup

  :after (eglot)
  :config
  (eglot-x-setup)

  :bind (:map eglot-mode-map
              ("M-S-<up>" . eglot-x-move-item-up)
              ("M-S-<down>" . eglot-x-move-item-down)))

(use-package comint
  :defer t
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
  "Pipe buffer from START to END through CMD.

Region to shell command and replace region with the output,
combined stdout and stderr.  The region is only replaced when the
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
  "Like plan9 sam x: x/REGEX/ FN.

Loops over each mach on regex, set point to start of the
match and applies start and end of the match to fn.

Examples:
\(x \"emacs\"
  (lambda (start end)
    (call-shell-region start end \"tr [e] [E]\" t (current-buffer))))

\(x \"macs\"
  (lambda (start end)
    (insert-char ?E)))

\(x \"vim\" delete-region)"
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
  "Sends filename to plumber."
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
  "Sends the range to the current tmux pane followed by Enter.

When region is active send from START to END."
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
  "Apply shell command uni on region.

When region is active apply from START to END."
  (interactive "r")
  (pipe-shell-region "uni" start end))
(keymap-global-set "C-c C-u" #'apply-uni-region)

(defun copy-buffer-name ()
  (interactive)
  (kill-new (if (buffer-file-name)
                (buffer-file-name)
              (buffer-name))))

(use-package move-text
  :functions
  move-text-default-bindings

  :config
  (move-text-default-bindings))

(use-package fzf
  :functions
  fzf-find-file-in-dir
  fzf-find-file)

(use-package rg)

(defun neosimsim-prog-mode-hook ()
  (setq indicate-empty-lines t)
  (setq show-trailing-whitespace t))

(use-package prog-mode
  :defer t
  :config
  (add-hook 'prog-mode-hook #'neosimsim-prog-mode-hook))

(use-package treesit
  :custom
  (treesit-font-lock-level 4))

(defun neosimsim-text-mode-hook ()
  (mixed-pitch-mode t)
  (setq indicate-empty-lines t)
  (setq show-trailing-whitespace t))

(use-package text-mode
  :defer t
  :functions
  mixed-pitch-mode

  :config
  (add-hook 'text-mode-hook #'neosimsim-text-mode-hook))

(defvar myenv-formatter "sed 's/[[:blank:]]*$//'"
  "Command used by `format-buffer'.")
(make-variable-buffer-local 'myenv-formatter)
(defun format-buffer ()
  "Format the current buffer using the shell command stored in `myenv-formatter'."
  (interactive)
  (let ((p (point))
        (prev-point-max (point-max)))
    (pipe-shell-region myenv-formatter (point-min) (point-max))
    (goto-char (+ p (- (point-max) prev-point-max)))))
(bind-key "C-x M-f" #'format-buffer)

(defun neosimsim-emacs-lisp-mode-hook ()
  (indent-tabs-mode -1))

(use-package elisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'neosimsim-emacs-lisp-mode-hook))

(defun haskell-setup ()
  (setq myenv-formatter "ormolu --no-cabal"))

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook #'haskell-setup))

(defun cabal-setup ()
  (setq myenv-formatter "cabal-fmt"))

(use-package haskell-cabal
  :defer t
  :config
  (add-hook 'haskell-cabal-mode-hook #'cabal-setup))

(defun fish-setup ()
  (setq myenv-formatter "fish_indent"))

(use-package fish-mode
  :defer t
  :config
  (add-hook 'fish-mode-hook #'fish-setup))

(defun elixir-setup ()
  (setq neosimsim-project-root-markers '(".git" "mix.lock")))

(use-package elixir-ts-mode
  :defer t
  :custom-face
  (elixir-ts-atom ((t (:inherit font-lock-builtin-face))))
  (elixir-ts-keyword-key ((t (:inherit font-lock-builtin-face))))
  (elixir-ts-comment-doc-attribute ((t (:inherit elixir-ts-attribute))))
  (elixir-ts-comment-doc-identifier ((t (:inherit elixir-ts-attribute))))

  :bind (:map elixir-ts-mode-map
              ([remap format-buffer] . elixir-format))

  :config
  (add-hook 'elixir-ts-mode-hook #'elixir-setup))

(defun term-setup ()
  (modify-syntax-entry ?: "_" term-mode-syntax-table)
  (modify-syntax-entry ?. "_" term-mode-syntax-table))

(use-package term
  :defer t
  :config
  (add-hook 'term-mode-hook #'term-setup)
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(defun rust-setup ()
  (setq neosimsim-project-root-markers '(".git" "Cargo.lock"))
  (setq myenv-formatter "rustfmt"))

(use-package rust-mode
  :defer t
  :defines
  rust-load-optional-libraries
  rust-mode-map

  :init
  (setq rust-load-optional-libraries t)

  :custom
  (rust-mode-treesitter-derive t)

  :config
  (add-hook 'rust-mode-hook #'rust-setup)

  :bind (:map rust-mode-map
              ([remap forward-paragraph] . rust-end-of-defun)
              ([remap backward-paragraph] . rust-beginning-of-defun)))

(use-package rustic
  :defer t
  :custom
  (rustic-lsp-client #'eglot)

  :config
  ;; Workaround for https://github.com/brotzeit/rustic/issues/573
  (setq rustic-compilation-panic '("thread '[^']+' panicked at \\([^\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))

(defun nix-setup ()
  (setq myenv-formatter "nixpkgs-fmt"))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-ts-mode-hook #'nix-setup))

(defun neosimsim-Info-mode-hook ()
  (mixed-pitch-mode t))

(use-package info
  :defer t
  :config
  (add-hook 'Info-mode-hook #'neosimsim-Info-mode-hook))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package buffer-move
  :bind (("C-S-<up>"    . buf-move-up)
         ("C-S-<down>"  . buf-move-down)
         ("C-S-<left>"  . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(defun neosimsim-python-mode-hook ()
  (setq neosimsim-project-root-markers '(".git" "pyproject.toml")))

(use-package python
  :config
  (add-hook 'python-ts-mode-hook #'neosimsim-python-mode-hook))

(use-package vc-hooks
  :custom
  (vc-display-status nil))

(use-package yaml-ts-mode)

(use-package yasnippet
  :hook ((eglot-mode . yas-minor-mode)))

(defun neosimsim-fzf (&optional prompt-dir)
  "Wrapper around `fzf-find-file'.

Calls `fzf-find-file' or `fzf-find-file-in-dir' depending on PROMPT-DIR
or when in a project.  (See `neosimsim-project-find-root')"
  (interactive "P")
  (if prompt-dir
      (fzf-find-file-in-dir)
    (pcase (neosimsim-project-find-root ".")
      (`nil (fzf-find-file))
      (`(,_ ,_ ,p-root) (fzf-find-file-in-dir p-root)))))

(bind-key "C-c C-c C-f" #'neosimsim-fzf)
