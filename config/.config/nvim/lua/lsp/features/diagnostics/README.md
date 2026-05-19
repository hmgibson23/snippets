# Diagnostic Suppression System

A comprehensive system for suppressing LSP diagnostics in Neovim with comment-based and storage-based approaches.

## Features

- **Provider-based architecture**: Works with LSP, Treesitter, and none-ls
- **Scope-based suppression**: Suppress at statement, function, class, file, or project level
- **Comment adapters**: Generate language-specific suppression comments (Python supported)
- **Storage system**: Persists suppressions to `.nvim/diagnostics.json`
- **Interactive UI**: `vim.ui.select` integration for scope selection and suppression management; Snacks enhances this when loaded

## Setup

Add to your Neovim config:

```lua
require('lsp.features.diagnostics').setup({
  -- Optional: custom config path (default: .nvim/diagnostics.json)
  config_path = vim.fn.getcwd() .. "/.nvim/diagnostics.json",
  
  -- Optional: customize keybindings
  keybindings = {
    prefix = '<leader>lx',  -- default prefix
    mappings = {
      suppress_inline = 's',      -- <leader>lxs
      suppress_above = 'S',       -- <leader>lxS
      suppress_file = 'f',        -- <leader>lxf
      unsuppress = 'u',           -- <leader>lxu
      suppress_interactive = 'i', -- <leader>lxi
      list_suppressions = 'l',    -- <leader>lxl
    }
  }
})
```

## Default Keybindings

| Keybinding | Action | Description |
|------------|--------|-------------|
| `<leader>lxs` | Suppress inline | Add suppression comment at end of line |
| `<leader>lxS` | Suppress above | Add suppression comment on line above |
| `<leader>lxf` | Suppress file | Add file-level suppression comment |
| `<leader>lxu` | Unsuppress | Remove suppression at cursor |
| `<leader>lxi` | Interactive | Show scope picker via `vim.ui.select`/Snacks |
| `<leader>lxl` | List | Show all suppressions via `vim.ui.select`/Snacks |

## Commands

- `:SuppressDiagnostic` - Suppress with inline comment
- `:SuppressDiagnosticAbove` - Suppress with comment above line
- `:SuppressDiagnosticFile` - Suppress at file level
- `:UnsuppressDiagnostic` - Remove suppression

## Python Support

The system automatically generates appropriate suppression comments for Python diagnostics:

```python
# pylint: disable=unused-variable
x = 1  # type: ignore[unused-variable]  # noqa: F841  # pyright: ignore[reportUnusedVariable]
```

Supports: pylint, mypy, pyright, ruff, flake8

## Architecture

```
lsp/features/diagnostics/
├── init.lua              # Main entry point
├── commands.lua          # User commands
├── keymaps.lua           # Keybinding setup
├── ui.lua                # Interactive UI components
├── manager.lua           # Core suppression manager
├── storage.lua           # Persistence layer
├── scope_detector.lua    # Treesitter-based scope detection
└── adapters/
    └── python.lua        # Python comment generation
```

## Testing

Run tests with:
```bash
nvim --headless -c "PlenaryBustedDirectory tests/lsp/ {minimal_init = 'tests/minimal_init.lua'}"
```

Current test coverage: 62 tests passing
- 25 provider tests
- 20 suppression tests
- 17 Python adapter tests
