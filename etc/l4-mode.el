;;; l4-mode.el --- Major mode and LSP support for L4 language -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SMU CCLAW

;; Author: SMU CCLAW
;; Keywords: languages, lsp
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This file provides L4 language support for Emacs with LSP integration.
;;
;; Installation (for Emacs Prelude users):
;;
;; 1. Make sure jl4-lsp is installed and on your PATH:
;;
;;      cabal install exe:jl4-lsp --overwrite-policy=always
;;
;; 2. Copy this file to your Prelude personal directory:
;;
;;      cp l4-mode.el ~/.emacs.d/personal/
;;
;; Prelude has lsp-mode preconfigured, so LSP will start automatically
;; when you open a .l4 file.
;;
;; Alternative: If you're not using Prelude or prefer eglot (built into
;; Emacs 29+), add this to your config:
;;
;;      (add-hook 'l4-mode-hook 'eglot-ensure)

;;; Code:

(require 'prog-mode)

;; Define L4 major mode
(define-derived-mode l4-mode prog-mode "L4"
  "Major mode for editing L4 language files."
  :group 'l4

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  ;; Indentation (L4 is layout-sensitive like Python)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)

  ;; Font lock (basic syntax highlighting)
  (setq-local font-lock-defaults
              '(l4-font-lock-keywords nil nil nil nil)))

;; Basic syntax highlighting
(defvar l4-font-lock-keywords
  (list
   ;; Keywords
   '("\\<\\(DECLARE\\|DEFINE\\|GIVEN\\|DECIDE\\|HOLDS\\|IF\\|THEN\\|ELSE\\|WHEN\\|AND\\|OR\\|NOT\\|TRUE\\|FALSE\\|CASE\\|IS\\|AS\\)\\>" . font-lock-keyword-face)
   ;; Types
   '("\\<\\(BOOLEAN\\|STRING\\|NUMBER\\|DATE\\|Time\\|Duration\\|Money\\|Person\\|Organization\\)\\>" . font-lock-type-face)
   ;; Function/rule names
   '("^\\([A-Za-z][A-Za-z0-9_]*\\)\\s-*:" . (1 font-lock-function-name-face))
   ;; Strings
   '("\"[^\"]*\"" . font-lock-string-face)
   ;; Numbers
   '("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face))
  "Syntax highlighting for L4 mode.")

;; File association
(add-to-list 'auto-mode-alist '("\\.l4\\'" . l4-mode))

;;; LSP Configuration

;; lsp-mode configuration (used by Prelude)
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(l4-mode . "l4"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "jl4-lsp")
    :major-modes '(l4-mode)
    :language-id "l4"
    :server-id 'jl4-lsp)))

;; Auto-start lsp-mode when opening L4 files
(add-hook 'l4-mode-hook #'lsp-deferred)

;; eglot configuration (alternative, built into Emacs 29+)
;; If you prefer eglot over lsp-mode, comment out the lsp-deferred hook above
;; and uncomment the following line:
;; (add-hook 'l4-mode-hook #'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(l4-mode . ("jl4-lsp"))))

(provide 'l4-mode)
;;; l4-mode.el ends here
