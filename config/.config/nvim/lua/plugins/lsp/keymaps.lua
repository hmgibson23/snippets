local keymaps = {}
local icons = require("config.icons")

local function snacks_picker(name, opts)
  return function()
    Snacks.picker[name](opts)
  end
end

keymaps.whichkey = {
  { "<leader>l", group = "[L]SP" },
  { "<leader>lv", group = "Venv" },
}

keymaps.maps = {
  { "K", vim.lsp.buf.hover, desc = "Show Hover Documentation" },
  { "gD", snacks_picker("lsp_declarations"), desc = "Goto Declaration" },
  { "gd", snacks_picker("lsp_definitions"), desc = "Goto Definition" },
  { "gr", snacks_picker("lsp_references"), desc = "References", nowait = true },
  { "gI", snacks_picker("lsp_implementations"), desc = "Goto Implementation" },
  { "gy", snacks_picker("lsp_type_definitions"), desc = "Goto T[y]pe Definition" },

  { "<leader>la", function() require("actions-preview").code_actions() end, desc = "Code Action", mode = { "n", "v" } },
  { "<leader>lc", vim.lsp.codelens.run, desc = "[C]odeLens", icon = icons.misc.Star },
  { "<leader>ld", "<cmd>DiagnosticPalette<cr>", desc = "[D]iagnostics palette" },
  { "<leader>lD", function() vim.diagnostic.enable(not vim.diagnostic.is_enabled()) end, desc = "Toggle [D]iagnostics" },
  { "<leader>lf", snacks_picker("lsp_definitions"), desc = "Definitions" },
  { "<leader>lF", function() vim.lsp.buf.format({ async = true }) end, desc = "[F]ormat Document" },
  { "<leader>li", "<cmd>LspInfo<CR>", desc = "Lsp [I]nfo" },
  { "<leader>lI", snacks_picker("lsp_implementations"), desc = "[I]mplementations" },
  { "<leader>ll", vim.lsp.codelens.run, desc = "Run CodeLens" },
  { "<leader>lL", vim.lsp.codelens.refresh, desc = "Refresh Code[L]ens" },
  { "<leader>ln", vim.lsp.buf.rename, desc = "Re[n]ame" },
  { "<leader>lq", "<cmd>Trouble qflist toggle<CR>", desc = "Trouble [Q]uickFix" },
  { "<leader>lr", snacks_picker("lsp_references"), desc = "[R]eferences" },
  { "<leader>lR", "<cmd>Trouble lsp_references toggle<cr>", desc = "Trouble [R]eferences" },
  { "<leader>ls", snacks_picker("lsp_symbols"), desc = "Document [S]ymbols" },
  { "<leader>lS", snacks_picker("lsp_workspace_symbols"), desc = "Workspace [S]ymbols" },
  { "<leader>lt", "<cmd>Trouble diagnostics toggle<CR>", desc = "[T]rouble diagnostics" },
  { "<leader>lT", snacks_picker("lsp_type_definitions"), desc = "[T]ype definitions" },
  { "<leader>lvs", "<cmd>VenvSelect<cr>", desc = "Select virtualenv" },
  { "<leader>lw", "<cmd>DocsViewToggle<CR>", desc = "Toggle Docs Vie[w]" },
}

function keymaps.setup(bufnr)
  for _, map in ipairs(keymaps.maps) do
    local modes = map.mode or "n"
    local opts = {
      buffer = bufnr,
      desc = "LSP: " .. map.desc,
      nowait = map.nowait,
    }
    vim.keymap.set(modes, map[1], map[2], opts)
  end
end

return keymaps
