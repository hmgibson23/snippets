return {
  {
    "echasnovski/mini.nvim",
    config = function()
      -- Minimal tabline
      require("mini.tabline").setup({
        -- Show file icons (requires mini.icons)
        show_icons = true,

        -- Set vim settings (always show tabline)
        set_vim_settings = true,

        -- Custom format function to show modified state
        format = function(buf_id, label)
          local modified = vim.bo[buf_id].modified and " ●" or ""
          return MiniTabline.default_format(buf_id, label) .. modified
        end,

        -- Show tabpage section on the left
        tabpage_section = "left",
      })

      -- Minimal session management
      local function get_cwd_as_name()
        local dir = vim.fn.getcwd(0)
        return dir:gsub("[^A-Za-z0-9]", "_")
      end

      require("mini.sessions").setup({
        autoread = false,
        autowrite = false,
        directory = vim.fn.stdpath("data") .. "/session", -- Global session directory
        file = "",                                     -- Disable cwd-local Session.vim files
        force = { read = false, write = true, delete = false },
        hooks = {
          pre = {
            write = function()
              -- Pre-write hook: save a task bundle before the session is written.
              local ok, overseer = pcall(require, "overseer")
              if not ok then
                return
              end
              overseer.save_task_bundle(
                get_cwd_as_name(),
                nil,                  -- Use default task selection options
                { on_conflict = "overwrite" } -- Overwrite existing bundle if needed.
              )
            end,
          },
          post = {
            write = function()
              -- Post-write hook: load the task bundle after the session is written.
              local ok, overseer = pcall(require, "overseer")
              if not ok then
                return
              end
              overseer.load_task_bundle(get_cwd_as_name(), { ignore_missing = true })
            end,
          },
        },
        verbose = { read = false, write = true, delete = true },
      })

      -- Bind project/session helpers early; heavier mini modules are deferred below.
      local wk_ok, which_key = pcall(require, "which-key")
      if wk_ok then
        which_key.add({
          { "gbp",       "<cmd>bp<cr>",       desc = "[P]revious Buffer" },
          { "gbn",       "<cmd>bn<cr>",       desc = "[N]ext Buffer" },

          { "<leader>p", group = "[P]rojects" },
          {
            "<leader>pc",
            "<cmd>mksession! " .. vim.fn.stdpath("data") .. "/session/" .. get_cwd_as_name() .. ".vim<cr>",
            desc = "[P]Make Session",
          },
          {
            "<leader>ps",
            function()
              require("mini.sessions").write(get_cwd_as_name() .. ".vim")
            end,
            desc = "[S]ave Session",
          },
        })
      end

      vim.api.nvim_create_autocmd("User", {
        pattern = "VeryLazy",
        once = true,
        callback = function()
          require("mini.animate").setup()
          require("mini.ai").setup({
            n_lines = 500,
          })
          require("mini.jump").setup({})
          require("mini.jump2d").setup({})
          require("mini.clue").setup({})
          require("mini.surround").setup({
            mappings = {
              add = "sa",       -- Add surrounding in Normal and Visual modes
              delete = "sd",    -- Delete surrounding
              find = "sf",      -- Find surrounding (to the right)
              find_left = "sF", -- Find surrounding (to the left)
              highlight = "sh", -- Highlight surrounding
              replace = "sr",   -- Replace surrounding
              update_n_lines = "sn", -- Update `n_lines`

              suffix_last = "l", -- Suffix to search with "prev" method
              suffix_next = "n", -- Suffix to search with "next" method
            },
          })
          require("mini.align").setup()
        end,
      })

      vim.api.nvim_create_autocmd("InsertEnter", {
        once = true,
        callback = function()
          require("mini.pairs").setup()
        end,
      })
    end,
  },
}
