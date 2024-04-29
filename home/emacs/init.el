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
                      (font-spec :family "Noto Color Emoji"))))

(use-package files
  :custom
  (auto-save-default nil)
  (make-backup-files nil)
  (major-mode-remap-alist
   '((elixir-mode . elixir-ts-mode))))

(use-package simple
  :custom
  (kill-whole-line t "Also delete new-line")
  (set-mark-command-repeat-pop t)
  (column-number-mode t "Also show column of point in info bar"))

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
  :bind (("C-c C-x C-o" . org-clock-out)
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
				  (alltodo "")))


				("n" "Next Tasks" todo "NEXT"
				 ((org-agenda-overriding-header "Next Tasks")))

				("w" "Work TODOs" tags-todo "work"
				 ((org-agenda-overriding-header "Work TODOs")))))

  (org-capture-templates '(("t" "Task" entry (file "~/notes/todo.org")
			    "" :empty-lines 1)
			   ("o" "Offenen Fragen" entry (clock)
			    "" :empty-lines 1))))


(use-package git-commit
  :defer t
  :config
  (defun my-git-commit-mode-hook ()
    (flyspell-mode))
  (add-hook 'git-commit-mode-hook #'my-git-commit-mode-hook))

(use-package magit
  :custom
  (magit-blame-echo-style 'show-lines
			  "Show commit info before chunks. The default value 'lines just shows an empty line"))

;; Ensure magit-extras for `magit-project-status'.
(use-package magit-extras
  :after (:any project magit))

(use-package ivy
  :demand t
  :config
  (ivy-mode t)

  :custom
  (ivy-extra-directories ())
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

  :bind (("C-c C-r" . ivy-resume)))

(use-package counsel
  :demand t
  :config
  (counsel-mode t)

  :custom
  (counsel-rg-base-command '("rg" "--max-columns" "240" "--with-filename" "--no-heading"
			     "--line-number" "--color" "never" "%s" "--hidden" "--glob" "!.git"))

  :bind (("C-c g" . counsel-git)
         ("C-c k" . counsel-rg)))

(use-package amx
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

(use-package project
  :defer t
  :config
  ;; Idea from https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
  (defcustom project-root-markers
    '(".git" "Cargo.lock" "mix.lock")
    "File or directories that indicate the root of a project.

Sometimes a git repo consist of multiple projects, this function looks
for special files and directories marking such project."
    :type '(repeat string)
    :group 'project)

  (defun project-root-p (path)
    "Check if PATH is a project root."
    (seq-find (lambda (marker) (file-exists-p (concat path marker))) project-root-markers))

  (defun project-find-root (path)
    "Search up from PATH for project root.

This functions takes into account that a sub-folder of a git repo might
be a sub-project root. In that case the sub-folder is returned as root
but if under version control, it is still marked as such, so that the
correct implementation for `project-files' is used and files ignored by
version controller are excluded."
    (when-let ((root (locate-dominating-file path #'project-root-p)))
      (if (project-try-vc root)
	  (list 'vc 'Git (expand-file-name root))
	(cons 'transient (expand-file-name root)))))

  (add-to-list 'project-find-functions #'project-find-root))

(use-package eglot
  :defer t
  :config
  (defun my-eglot-managed-mode-hook ()
    (eglot-inlay-hints-mode -1))
  (add-hook 'eglot-managed-mode-hook #'my-eglot-managed-mode-hook)

  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls"))))

(use-package eglot-x
  :after (eglot)
  :config
  (eglot-x-setup))

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
(x \"emacs\"
  (lambda (start end)
    (call-shell-region start end \"tr [e] [E]\" t (current-buffer))))

(x \"macs\"
  (lambda (start end)
    (insert-char ?E)))

(x \"vim\" delete-region)
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
  :config
  (move-text-default-bindings))

(use-package prog-mode
  :defer t
  :config
  (defun my-prog-mode-hook ()
    (setq indicate-empty-lines t)
    (setq show-trailing-whitespace t)
    (display-line-numbers-mode t))
  (add-hook 'prog-mode-hook #'my-prog-mode-hook))

(use-package text-mode
  :defer t
  :config
  (defun my-text-mode-hook ()
    (mixed-pitch-mode t)
    (setq indicate-empty-lines t)
    (setq show-trailing-whitespace t))
  (add-hook 'text-mode-hook #'my-text-mode-hook))

(defvar myenv-formatter "sed 's/[[:blank:]]*$//'"
  "Commands used by format-buffer")
(make-variable-buffer-local 'myenv-formatter)
(defun format-buffer ()
  "Format the current buffer using the shell command stored in `myenv-formatter'."
  (interactive)
  (let ((p (point))
        (prev-point-max (point-max)))
    (pipe-shell-region myenv-formatter (point-min) (point-max))
    (goto-char (+ p (- (point-max) prev-point-max)))))
(keymap-global-set "C-x M-f" #'format-buffer)

(use-package haskell-mode
  :defer t
  :config
  (defun haskell-setup ()
    (setq myenv-formatter "ormolu --no-cabal"))
  (add-hook 'haskell-mode-hook #'haskell-setup))

(use-package haskell-cabal
  :defer t
  :config
  (defun cabal-setup ()
    (setq myenv-formatter "cabal-fmt"))
  (add-hook 'haskell-cabal-mode-hook #'cabal-setup))

(use-package fish-mode
  :defer t
  :config
  (defun fish-setup ()
    (setq myenv-formatter "fish_indent"))
  (add-hook 'fish-mode-hook #'fish-setup))

(use-package elixir-ts-mode
  :custom-face
  (elixir-ts-atom ((t (:inherit font-lock-builtin-face))))
  (elixir-ts-keyword-key ((t (:inherit font-lock-builtin-face))))
  (elixir-ts-comment-doc-attribute ((t (:inherit elixir-ts-attribute))))
  (elixir-ts-comment-doc-identifier ((t (:inherit elixir-ts-attribute)))))

(use-package elixir-mode
  :defer t
  :config
  (defun elixir-setup ()
    (local-set-key (kbd "C-x M-f") #'elixir-format)
    (modify-syntax-entry ?& "." elixir-mode-syntax-table))
  (add-hook 'elixir-mode-hook #'elixir-setup))

(use-package term
  :defer t
  :config
  (defun term-setup ()
    (modify-syntax-entry ?: "_" term-mode-syntax-table)
    (modify-syntax-entry ?. "_" term-mode-syntax-table))
  (add-hook 'term-mode-hook #'term-setup)
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package elpy
  ;; currently fails to install with nix, because of
  ;; Error: Wrong type argument: arrayp, ...
  :disabled t
  :defer t
  :config
  (defun python-setup ()
    (elpy-enable)
    (local-set-key (kbd "C-x M-f") #'elpy-format-code)))

(use-package rust-mode
  :defer t
  :config
  (defun rust-setup ()
    (setq myenv-formatter "rustfmt"))
  (add-hook 'rust-mode-hook #'rust-setup))

(use-package rustic
  :defer t
  :custom
  (rustic-lsp-client #'eglot))

(use-package nix-mode
  :mode "\\.nix\\'"

  :config
  (defun nix-setup ()
    (setq myenv-formatter "nixpkgs-fmt"))
  (add-hook 'nix-mode-hook #'nix-setup))

(use-package info
  :defer t
  :config
  (defun my-Info-mode-hook ()
    (mixed-pitch-mode t))
  (add-hook 'Info-mode-hook #'my-Info-mode-hook))

(use-package org
  :defer t
  :config

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

(use-package ox-html
  :defer t
  :custom
  (org-html-checkbox-type 'unicode))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package buffer-move
  :bind (("C-S-<up>"    . buf-move-up)
	 ("C-S-<down>"  . buf-move-down)
	 ("C-S-<left>"  . buf-move-left)
	 ("C-S-<right>" . buf-move-right)))

(use-package elisp-format)
