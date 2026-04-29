-- ABOUTME: Default configuration for LSP enhancement features
-- ABOUTME: Provides sensible defaults for diagnostics, actions, and UI settings

return {
  diagnostics = {
    -- Virtual text settings
    virtual_text = {
      enabled = true,
      prefix = "■",
      spacing = 4,
    },

    -- Suppression defaults
    suppression = {
      enabled = true,
      persist_location = ".nvim/diagnostics.json",
      default_scope = "statement", -- IntelliJ starts at statement level
    },

    -- Inspection menu defaults
    inspection = {
      default_state = "enabled", -- All sources enabled by default
      show_disabled_sources = true,
      keybind = "<leader>ld",
    },
  },

  actions = {
    -- Indicator settings
    indicators = {
      enabled = true,
      icon = "💡",
      virtual_text = true,
      sign_column = false,
    },

    -- Preview settings
    preview = {
      enabled = true,
      show_diff = true,
      backend = "telescope", -- or "native"
    },
  },
}
