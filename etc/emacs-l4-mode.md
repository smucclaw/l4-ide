# Emacs Configuration for L4

This directory contains Emacs configuration for L4 language support with LSP integration.

## Prerequisites

1. **Emacs Prelude** - This guide assumes you're using [Emacs Prelude](https://github.com/bbatsov/prelude)

2. **Install jl4-lsp**:

   ```bash
   cabal install exe:jl4-lsp --overwrite-policy=always
   ```

3. **Ensure jl4-lsp is on your PATH**:
   ```bash
   which jl4-lsp
   # Should output something like: ~/.cabal/bin/jl4-lsp
   ```

## Installation

Copy `l4-mode.el` to your Prelude personal directory:

```bash
cp l4-mode.el ~/.emacs.d/personal/
```

Prelude automatically loads all `.el` files in `~/.emacs.d/personal/`, and lsp-mode is preconfigured. That's it—open any `.l4` file and LSP will start automatically.

## Features

Once configured, you'll have access to:

- **Syntax highlighting** for L4 keywords and types
- **Diagnostics** (errors and warnings) as you type
- **Go to definition** (`M-.`)
- **Hover documentation** (hover over symbols to see their types)
- **Code completion**
- **Code actions** (quick fixes and refactorings)

## Usage

1. Open any `.l4` file
2. L4 mode will activate automatically
3. The LSP server will start and provide features

## Troubleshooting

### LSP server not starting

Check that `jl4-lsp` is on your PATH:

```bash
which jl4-lsp
```

If it's not found, add your Cabal bin directory to your PATH. Create or edit `~/.emacs.d/personal/preload/path-config.el`:

```elisp
(setenv "PATH" (concat (getenv "HOME") "/.cabal/bin:" (getenv "PATH")))
(add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin"))
```

### Custom jl4-lsp location

If you need to specify a custom path to jl4-lsp, add to your personal config:

```elisp
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "/custom/path/to/jl4-lsp")
    :major-modes '(l4-mode)
    :language-id "l4"
    :server-id 'jl4-lsp)))
```

### Viewing LSP logs

- View logs: `M-x lsp-workspace-show-log`
- View workspace: `M-x lsp-describe-session`

## Using eglot instead of lsp-mode

If you're not using Prelude, or prefer eglot (built into Emacs 29+), add to your config:

```elisp
(add-hook 'l4-mode-hook 'eglot-ensure)
```

For eglot debugging:

- View events: `M-x eglot-events-buffer`
- View stderr: `M-x eglot-stderr-buffer`

## Customization

Add customizations to `~/.emacs.d/personal/l4-config.el`:

```elisp
;; Disable code lenses ("Visualize | Simplify and Visualize" links)
(add-hook 'l4-mode-hook (lambda () (setq-local lsp-lens-enable nil)))

;; Change indentation width (default: 2 spaces)
(add-hook 'l4-mode-hook
          (lambda ()
            (setq tab-width 4)))

;; Enable line numbers
(add-hook 'l4-mode-hook 'display-line-numbers-mode)
```

## Contributing

If you improve this Emacs configuration, please contribute it back to the project!
