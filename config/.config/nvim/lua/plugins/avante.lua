return {
  "yetone/avante.nvim",
  build = "make",
  event = "VeryLazy",
  version = false,
  opts = {
    instructions_file = "avante.md",
    provider = "copilot",
    providers = {
      copilot = {
        endpoint = "https://api.githubcopilot.com",
        model = "gpt-4o-2024-05-13",
        timeout = 30000,
      },
    },
    behaviour = {
      auto_set_keymaps = true,
    },
    mappings = {
      ask = "<leader>aaa",
      new_ask = "<leader>aan",
      full_view_ask = "<leader>aam",
      edit = "<leader>aae",
      refresh = "<leader>aar",
      focus = "<leader>aaf",
      stop = "<leader>aaS",
      toggle = {
        default = "<leader>aat",
        debug = "<leader>aad",
        selection = "<leader>aaC",
        suggestion = "<leader>aas",
        repomap = "<leader>aaR",
      },
      files = {
        add_current = "<leader>aac",
        add_all_buffers = "<leader>aaB",
      },
      select_model = "<leader>aa?",
      select_history = "<leader>aah",
    },
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
    "MunifTanjim/nui.nvim",
    "echasnovski/mini.pick",
    "nvim-telescope/telescope.nvim",
    "hrsh7th/nvim-cmp",
    "ibhagwan/fzf-lua",
    "stevearc/dressing.nvim",
    "folke/snacks.nvim",
    "nvim-tree/nvim-web-devicons",
    "zbirenbaum/copilot.lua",
    {
      "HakonHarnes/img-clip.nvim",
      event = "VeryLazy",
      opts = {
        default = {
          embed_image_as_base64 = false,
          prompt_for_file_name = false,
          drag_and_drop = {
            insert_mode = true,
          },
        },
      },
    },
    {
      "MeanderingProgrammer/render-markdown.nvim",
      opts = {
        file_types = { "markdown", "Avante" },
      },
      ft = { "markdown", "Avante" },
    },
  },
}
