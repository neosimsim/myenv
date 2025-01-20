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
                      (font-spec :family "Noto Color Emoji")))

  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'none)
    (setq mac-command-modifier 'meta)))

(use-package files
  :custom
  (auto-save-default nil)
  (make-backup-files nil)
  (major-mode-remap-alist
   '((elixir-mode . elixir-ts-mode)
     (js-json-mode . json-ts-mode)
     (css-mode . css-ts-mode)
     (python-mode . python-ts-mode))))

(use-package simple
  :custom
  (kill-whole-line t "Also delete new-line")
  (set-mark-command-repeat-pop t)
  (column-number-mode t "Also show column of point in info bar"))

(use-package minibuffer
  :custom
  (completion-styles '(basic partial-completion emacs22)))

(use-package custom
  :config
  (setq custom-file "~/.config/emacs-custom.el")
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package em-term
  :defer t
  :defines eshell-mode-map

  :config
  (add-to-list 'eshell-visual-subcommands '("git" . ("log" "diff" "show")))
  (add-to-list 'eshell-visual-options '("git" . ("--help" "--paginate")))

  (add-to-list 'eshell-visual-subcommands '("nix" . ("build" "shell" "flake")))
  (add-to-list 'eshell-visual-options '("nix" . ("--help")))

  (add-to-list 'eshell-visual-commands "home-manager"))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-g M-n" . flymake-goto-next-error)
              ("M-g M-p" . flymake-goto-prev-error)))

(use-package face-remap
  :if (eq system-type 'gnu/linux)
  :custom-face
  (variable-pitch ((t (:height 1.15 :family "Free Sans")))))

(use-package display-line-numbers
  :hook
  prog-mode
  yaml-ts-mode)

(use-package mixed-pitch
  :hook
  (Info-mode . mixed-pitch-mode)
  (org-mode . mixed-pitch-mode)
  (makdown-mode . mixed-pitch-mode)

  :custom
  (mixed-pitch-set-height t))

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-j" . org-clock-goto)
         ("C-c C-x C-x" . org-clock-in-last)
         ([remap org-set-tags-command] . counsel-org-tag))

  :custom
  (org-export-backends '(ascii html icalendar latex md odt))
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

  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)

  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s%b")
                              (todo . " %i %-12:c%b")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))

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
     (org . t)
     (calc . t)
     (shell . t)
     (scheme . t))))

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
  "Personal hook for `git-commit-mode-hook'."
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

(use-package modus-operandi-theme
  :config
  (load-theme 'modus-operandi t))

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
  "Personal hook for `eglot-managed-mode-hook'."
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

  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)))))

  :bind (:map eglot-mode-map
              ([remap neosimsim-format-buffer] . eglot-format-buffer)
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
  (let ((proc-buffer (get-buffer-create (format "*Pipe Shell Region Output: %s*" cmd)))
        (prev-point (point)))
    (with-current-buffer proc-buffer
      (delete-region (point-min) (point-max)))
    (if (equal 0 (call-shell-region start end cmd nil proc-buffer))
        (progn (unless (string=
                        (buffer-substring-no-properties start end)
                        (with-current-buffer proc-buffer (buffer-string)))
                 (delete-region start end)
                 (insert-buffer-substring proc-buffer)
                 (goto-char prev-point))
               (let ((proc-window (get-buffer-window proc-buffer)))
                 (when proc-window (quit-window nil proc-window)))
               (kill-buffer proc-buffer))
      (display-buffer proc-buffer))))

(defun track ()
  "Open today's tracking file."
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
  "Open new timux window in git root."
  (interactive)
  (let ((git-root (substring (shell-command-to-string "git rev-parse --show-toplevel") 0 -1)))
    (call-process "tmux" nil nil nil "new-window" "-c" git-root)))

(defun tmux-cwd ()
  "Open new tmux window in `default-directory'."
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

(defun neosimsim-copy-buffer-file-or-name ()
  "Puts the current buffer file-name in killring.

If buffer is not associated with a file the buffer name is used."
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

(use-package rg
  :defer t
  :config
  (rg-enable-default-bindings)

  :bind (("C-c s" . rg-menu)))

(defun neosimsim-prog-mode-hook ()
  "Personal hook for `prog-mode-hook'."
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
  "Personal hook for `text-mode-hook'."
  (setq indicate-empty-lines t)
  (setq show-trailing-whitespace t))

(use-package text-mode
  :defer t

  :config
  (add-hook 'text-mode-hook #'neosimsim-text-mode-hook))

(defvar-local neosimsim-formatter "sed 's/[[:blank:]]*$//'"
  "Shell command used by `neosimsim-format-buffer'.")

(defun neosimsim-format-buffer ()
  "Format the current buffer using the `neosimsim-formatter'."
  (interactive)
  (pipe-shell-region neosimsim-formatter (point-min) (point-max)))

(bind-key "C-x M-f" #'neosimsim-format-buffer)

(defun neosimsim-emacs-lisp-mode-hook ()
  "Personal hook for `emacs-lisp-mode-hook'."
  (indent-tabs-mode -1))

(use-package elisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'neosimsim-emacs-lisp-mode-hook))

(defun haskell-setup ()
  "Personal hook for `haskell-mode-hook'."
  (setq neosimsim-formatter "ormolu --no-cabal"))

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook #'haskell-setup))

(defun cabal-setup ()
  "Personal hook for `haskell-cabal-mode-hook'."
  (setq neosimsim-formatter "cabal-fmt"))

(use-package haskell-cabal
  :defer t
  :config
  (add-hook 'haskell-cabal-mode-hook #'cabal-setup))

(defun fish-setup ()
  "Personal hook for `fish-mode-hook'."
  (setq neosimsim-formatter "fish_indent"))

(use-package fish-mode
  :defer t
  :config
  (add-hook 'fish-mode-hook #'fish-setup))

(defun elixir-setup ()
  "Personal hook for `elixir-mode-hook'."
  (setq neosimsim-project-root-markers '(".git" "mix.lock")))

(use-package elixir-ts-mode
  :defer t
  :custom-face
  (elixir-ts-atom ((t (:inherit font-lock-builtin-face))))
  (elixir-ts-keyword-key ((t (:inherit font-lock-builtin-face))))
  (elixir-ts-comment-doc-attribute ((t (:inherit elixir-ts-attribute))))
  (elixir-ts-comment-doc-identifier ((t (:inherit elixir-ts-attribute))))

  :bind (:map elixir-ts-mode-map
              ([remap neosimsim-format-buffer] . elixir-format))

  :config
  (add-hook 'elixir-ts-mode-hook #'elixir-setup))

(defun term-setup ()
  "Personal hook for `term-mode-hook'."
  (modify-syntax-entry ?: "_" term-mode-syntax-table)
  (modify-syntax-entry ?. "_" term-mode-syntax-table))

(use-package term
  :defer t
  :config
  (add-hook 'term-mode-hook #'term-setup)
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(defun rust-setup ()
  "Personal hook for `rust-mode-hook'."
  (setq neosimsim-project-root-markers '(".git" "Cargo.lock"))
  (setq neosimsim-formatter "rustfmt"))

(use-package rustic
  :defer t
  :after rust-mode
  :defines
  rust-load-optional-libraries
  rust-mode-map
  rustic-compilation-panic

  :init
  (setq rust-load-optional-libraries t)


  :bind (:map rustic-mode-map
              ("<f5>" . rustic-cargo-check))

  :custom
  (rust-mode-treesitter-derive t)
  (rustic-lsp-client #'eglot)

  :config
  (add-hook 'rust-mode-hook #'rust-setup)
  ;; Workaround for https://github.com/brotzeit/rustic/issues/573
  (setq rustic-compilation-panic '("thread '[^']+' panicked at \\([^\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))


(defun nix-setup ()
  "Personal hook for `nix-mode-hook'."
  (setq neosimsim-formatter "nixpkgs-fmt"))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-ts-mode-hook #'nix-setup))

(defun go-setup ()
  "Personal hook for `go-ts-mode-hook'."
  (setq neosimsim-project-root-markers '("go.mod"))
  (setq neosimsim-formatter "gofmt"))

(use-package go-ts-mode
  :config
  (add-hook 'go-ts-mode-hook #'go-setup))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package buffer-move
  :bind (("C-S-<up>"    . buf-move-up)
         ("C-S-<down>"  . buf-move-down)
         ("C-S-<left>"  . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(defun neosimsim-python-mode-hook ()
  "Personal hook for `python-mode-hook'."
  (setq neosimsim-project-root-markers '(".git" "pyproject.toml")))

(use-package python
  :config
  (add-hook 'python-ts-mode-hook #'neosimsim-python-mode-hook))

(use-package vc-hooks
  :custom
  (vc-display-status nil))

(use-package dhall-mode
  :custom
  (dhall-format-arguments '("--unicode")))

(use-package yaml-ts-mode)

(defun neosimsim-json-setup ()
  "Personal hook for `json-mode-hook'."
  (setq neosimsim-formatter "jq ."))

(use-package json-ts-mode
  :config
  (add-hook 'json-ts-mode-hook #'neosimsim-json-setup))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package combobulate
  :hook
  json-ts-mode
  yaml-ts-mode
  go-ts-mode)

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

(use-package ansi-color
  :functions
  ansi-color-apply-on-region)

(defun neosimsim-colorize-compilation-buffer ()
  "`compilation-filter-hook' to apply ANSI color codes."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defconst neosimsim-path-locus-regex "\\(\\([[:alnum:]/._]+\\):\\([[:digit:]]+\\)\\(:\\([[:digit:]]+\\)?\\)?\\)")

(use-package compile
  :config
  (add-hook 'compilation-filter-hook #'neosimsim-colorize-compilation-buffer)

  (add-to-list 'compilation-error-regexp-alist-alist
               `(path-locus ,neosimsim-path-locus-regex 2 3 5 0 1))

  (add-to-list 'compilation-error-regexp-alist 'path-locus))
