local keymaps = {}
local icons = require("plugins.icons")

keymaps.whichkey = {
  -- BufferLine mappings
  { "<leader>l",   group = "[L]SP" }, -- group
  -- Venv mappings
  { "<leader>lv",  group = "Venv" },
  { "<leader>lvs", "<cmd>VenvSelect<cr>",                    desc = "Select" },

  -- LSP mappings
  { "<leader>lR",  "<cmd>Trouble lsp_references toggle<cr>", desc = "Trouble References" },
  { "<leader>lw",  "<cmd>DocsViewToggle<CR>",                desc = "Toggle Docs Vie[w]" },
  { "<leader>lO",  "<cmd>Outline<CR>",                       desc = "[O]utline" },
  {
    "<leader>lc",
    "<cmd>lua vim.lsp.codelens.run()<CR>",
    desc = "[C]odeLens",
    icon = icons.misc.Star,
  },
  {
    "<leader>lgd",
    "<cmd>Glance definitions<CR>",
    desc = "[G]lance [D]efinitions",
  },
  {
    "<leader>lgr",
    "<cmd>Glance references<CR>",
    desc = "[G]lance [R]eferences",
  },
  { "<leader>lgt", "<cmd>Glance type_definitions<CR>", desc = "[G]lance [T]ypes" },
  {
    "<leader>lgm",
    "<cmd>Glance implementations<CR>",
    desc = "[G]lance I[m]plementations",
  },
  { "<leader>lf",  "<cmd>Lspsaga finder<CR>",          desc = "[F]inder" },
  { "<leader>li",  "<cmd>LspInfo<CR>",                 desc = "Lsp [I]nfo" },
  {
    "<leader>ln",
    "<cmd>lua vim.lsp.buf.rename()<CR>",
    desc = "Re[n]ame",
  },
  { "<leader>ls", "<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>", desc = "Document [S]ymbols" },
  { "<leader>lt", "<cmd>Trouble diagnostics toggle<CR>",                              desc = "[T]rouble" },
  { "<leader>lq", "<cmd>Trouble qflist toggle<CR>",                                   desc = "Trouble [Q]uickFix" },
  { "<leader>lI", "<cmd>Trouble implementations toggle<CR>",                          desc = "Trouble QuickF[i]x" },
  { "<leader>lL", "<cmd>lua vim.lsp.codelens.refresh()<CR>",                          desc = "Refresh Code[L]ens" },
  { "<leader>ll", "<cmd>lua vim.lsp.codelens.run()<CR>",                              desc = "Run CodeLens" },
  {
    "<leader>lD",
    "<cmd>lua require('plugins.lsp').toggle_diagnostics()<CR>",
    desc = "Toggle Inline [D]iagnostics",
  },
  { "<leader>le",  "<cmd>lua require('aerial').toggle()<CR>",                   desc = "Toggle A[e]rial" },
  { "<leader>lb",  group = "[B oo" },
  { "<leader>lbo", "<cmd>lua require('boo').boo()<CR>",                         desc = "Show b[o]o" },
  { "<leader>lbc", "<cmd>lua require('boo').close()<CR>",                       desc = "[C]lose boo" },
  { "<leader>lF",  "<cmd>lua vim.lsp.buf.format({async = true})<CR>",           desc = "[F]ormat Document" },
  { "<leader>tp",  "<cmd>TSPlay<cr>",                                           desc = "TS[P]lay" },

  -- Additional Neotest mappings
  { "<leader>k",   group = "Neotest" },
  { "<leader>kt",  group = "Test Commands" },
  { "<leader>ka",  "<cmd>lua require('neotest').run.attach()<cr>",              desc = "Attach" },
  { "<leader>kA",  "<cmd>lua require('neotest').run.run({ suite = true })<cr>", desc = "Run all" },

  -- Visual mode mapping (you can include mode if needed)
  {
    "<leader>la",
    "<cmd>'<,'>lua vim.lsp.buf.range_code_action()<CR>",
    desc = "Code Action",
    mode = "v",
  },
}

return keymaps
