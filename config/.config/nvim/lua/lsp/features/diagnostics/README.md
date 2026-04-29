# Diagnostic Suppression System

A comprehensive system for suppressing LSP diagnostics in Neovim with comment-based and storage-based approaches.

## Features

- **Provider-based architecture**: Works with LSP, Treesitter, and none-ls
- **Scope-based suppression**: Suppress at statement, function, class, file, or project level
- **Comment adapters**: Generate language-specific suppression comments (Python supported)
- **Storage system**: Persists suppressions to `.nvim/diagnostics.json`
- **Interactive UI**: Telescope integration for scope selection and suppression management

## Setup

Add to your Neovim config:

```lua
require('lsp.features.diagnostics').setup({
  -- Optional: custom config path (default: .nvim/diagnostics.json)
  config_path = vim.fn.getcwd() .. "/.nvim/diagnostics.json",
  
  -- Optional: customize keybindings
  keybindings = {
    prefix = '<leader>d',  -- default prefix
    mappings = {
      suppress_inline = 's',      -- <leader>ds
      suppress_above = 'S',       -- <leader>dS
      suppress_file = 'f',        -- <leader>df
      unsuppress = 'u',           -- <leader>du
      suppress_interactive = 'i', -- <leader>di
      list_suppressions = 'l',    -- <leader>dl
    }
  }
})
```

## Default Keybindings

| Keybinding | Action | Description |
|------------|--------|-------------|
| `<leader>ds` | Suppress inline | Add suppression comment at end of line |
| `<leader>dS` | Suppress above | Add suppression comment on line above |
| `<leader>df` | Suppress file | Add file-level suppression comment |
| `<leader>du` | Unsuppress | Remove suppression at cursor |
| `<leader>di` | Interactive | Show scope picker (requires Telescope) |
| `<leader>dl` | List | Show all suppressions (requires Telescope) |

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
