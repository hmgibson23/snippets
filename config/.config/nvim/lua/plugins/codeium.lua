return {
  "jcdickinson/codeium.nvim",
  event = "InsertEnter",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "hrsh7th/nvim-cmp",
  },
  opts = {
    enable_chat = true,
    virtual_text = {
      enabled = false,
      manual = false,
      filetypes = {},
      default_filetype_enabled = true,
      idle_delay = 75,
      virtual_text_priority = 65535,
      map_keys = true,
      accept_fallback = nil,
      key_bindings = {
        accept = "<M-Tab>",
        accept_word = false,
        accept_line = false,
        clear = false,
        next = "<M-]>",
        prev = "<M-[>",
      },
    },
  },
}
