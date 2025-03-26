return {
  "nvim-lualine/lualine.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
    {
      "linrongbin16/lsp-progress.nvim",
      config = function()
        require("lsp-progress").setup({})
      end,
    },
  },
  config = function()
    local lualine = require("lualine")

    -- Define colors
    local colors = {
      blue = "#80a0ff",
      cyan = "#79dac8",
      black = "#080808",
      white = "#c6c6c6",
      red = "#ff5189",
      green = "#3B7A57", -- Terre Verte
      grey = "#303030",
    }

    -- Theme with rounded separators and gaps between sections
    local bubbles_theme = {
      normal = {
        a = { fg = colors.black, bg = colors.green },
        b = { fg = colors.white, bg = colors.grey },
        c = { fg = colors.white },
      },
      insert = { a = { fg = colors.black, bg = colors.blue } },
      visual = { a = { fg = colors.black, bg = colors.cyan } },
      replace = { a = { fg = colors.black, bg = colors.red } },
      inactive = {
        a = { fg = colors.white, bg = colors.black },
        b = { fg = colors.white, bg = colors.black },
        c = { fg = colors.white },
      },
    }

    -- Project Name Display with Icon
    local function project_name_display()
      local projections_available, Session = pcall(require, "projections.session")
      if projections_available then
        local info = Session.info(vim.loop.cwd())
        if info ~= nil then
          return " " .. info.project.name
        end
      end
      return " " .. vim.fs.basename(vim.loop.cwd()) -- Default icon
    end

    -- Function to get Codeium status
    local function codeium_status()
      local status = require("codeium.virtual_text").status()
      if status.state == "idle" then
        return " "
      elseif status.state == "waiting" then
        return "󰛂 Waiting..."
      elseif status.state == "completions" and status.total > 0 then
        return string.format(" %d/%d", status.current, status.total)
      end
      return " 0 "
    end

    -- Function to get Supermaven status
    local function supermaven_status()
      local sm = require("supermaven")
      local status = sm.status()
      if status.state == "idle" then
        return "󰢤 Idle"
      elseif status.state == "loading" then
        return "󰣐 Loading..."
      elseif status.state == "active" then
        return "󰒲 Active"
      end
      return "󰦖 Unknown"
    end

    -- Ensure Overseer is loaded
    local ok_overseer, overseer = pcall(require, "overseer")
    if not ok_overseer then
      return
    end

    lualine.setup({
      options = {
        theme = bubbles_theme,
        component_separators = { left = " ", right = " " }, -- Add spacing between components
        section_separators = { left = " ", right = " " }, -- Rounded separators with spacing
      },
      sections = {
        lualine_a = { { "mode", separator = { left = "", right = " " }, right_padding = 2 } },
        lualine_b = { { "filename", icon = "󰈙" }, { "branch", icon = "" } },
        lualine_c = {
          function()
            return require("lsp-progress").progress()
          end,
        },
        lualine_x = {
          codeium_status,
          supermaven_status,
          project_name_display,
          {
            "overseer",
            label = "",
            colored = true,
            symbols = {
              [overseer.STATUS.FAILURE] = "󰇸 ",
              [overseer.STATUS.CANCELED] = " ",
              [overseer.STATUS.SUCCESS] = " ",
              [overseer.STATUS.RUNNING] = " ",
            },
            unique = false,
          },
        },
        lualine_y = { { "filetype", icon = "" }, { "progress", icon = "󰦖" } },
        lualine_z = { { "location", separator = { left = "", right = " " }, left_padding = 2 } },
      },
    })

    vim.api.nvim_create_augroup("lualine_augroup", { clear = true })
    vim.api.nvim_create_autocmd("User", {
      group = "lualine_augroup",
      pattern = "LspProgressStatusUpdated",
      callback = require("lualine").refresh,
    })
  end,
}
