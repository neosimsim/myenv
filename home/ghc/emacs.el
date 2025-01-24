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
