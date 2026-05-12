local parsers = {
  "bash",
  "c",
  "c_sharp",
  "diff",
  "html",
  "lua",
  "luadoc",
  "markdown",
  "markdown_inline",
  "query",
  "vim",
  "vimdoc",
  "teal",
}

local function has_parser(lang)
  return pcall(vim.treesitter.language.add, lang)
end

local function start_treesitter(bufnr)
  bufnr = bufnr or 0
  if vim.bo[bufnr].buftype ~= "" then
    return
  end

  local filetype = vim.bo[bufnr].filetype
  local lang = vim.treesitter.language.get_lang(filetype)
  if not lang or not has_parser(lang) then
    return
  end

  pcall(vim.treesitter.start, bufnr, lang)

  local ok = pcall(require, "nvim-treesitter")
  if ok then
    vim.bo[bufnr].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end
end

return {
  {
    -- Neovim 0.12 requires the rewritten nvim-treesitter main branch. The old
    -- master branch ships queries that can crash the built-in highlighter with
    -- `attempt to call method 'range' (a nil value)`.
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    lazy = false,
    build = function()
      if vim.fn.executable("tree-sitter") == 1 then
        vim.cmd("TSUpdate")
      end
    end,
    dependencies = {
      { "nvim-treesitter/nvim-treesitter-textobjects", branch = "main" },
      { "windwp/nvim-ts-autotag", event = "InsertEnter" },
      { "JoosepAlviste/nvim-ts-context-commentstring", event = "BufReadPre" },
      { "p00f/nvim-ts-rainbow", enabled = false },
      { "RRethy/nvim-treesitter-textsubjects", enabled = false },
      { "nvim-treesitter/playground", cmd = { "TSPlaygroundToggle" }, enabled = false },
      { "nvim-treesitter/nvim-treesitter-context", event = "BufReadPre", enabled = false },
      { "mfussenegger/nvim-treehopper", enabled = false },
      {
        "m-demare/hlargs.nvim",
        event = "BufReadPre",
        opts = {},
      },
    },
    opts = {
      install_dir = vim.fn.stdpath("data") .. "/site",
    },
    config = function(_, opts)
      require("nvim-treesitter").setup(opts)

      if vim.fn.executable("tree-sitter") == 1 then
        local installed = require("nvim-treesitter.config").get_installed()
        local missing = vim.tbl_filter(function(parser)
          return not vim.tbl_contains(installed, parser)
        end, parsers)
        if #missing > 0 then
          require("nvim-treesitter").install(missing)
        end
      end

      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("UserTreesitterStart", { clear = true }),
        callback = function(args)
          start_treesitter(args.buf)
        end,
      })
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    keys = {
      {
        "af",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@function.outer", "textobjects")
        end,
        mode = { "x", "o" },
        desc = "Around function",
      },
      {
        "if",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@function.inner", "textobjects")
        end,
        mode = { "x", "o" },
        desc = "Inside function",
      },
      {
        "ac",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@class.outer", "textobjects")
        end,
        mode = { "x", "o" },
        desc = "Around class",
      },
      {
        "ic",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@class.inner", "textobjects")
        end,
        mode = { "x", "o" },
        desc = "Inside class",
      },
      {
        "]m",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_start("@function.outer", "textobjects")
        end,
        mode = { "n", "x", "o" },
        desc = "Next function start",
      },
      {
        "[m",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_start("@function.outer", "textobjects")
        end,
        mode = { "n", "x", "o" },
        desc = "Previous function start",
      },
      {
        "]]",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_start("@class.outer", "textobjects")
        end,
        mode = { "n", "x", "o" },
        desc = "Next class start",
      },
      {
        "[[",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_start("@class.outer", "textobjects")
        end,
        mode = { "n", "x", "o" },
        desc = "Previous class start",
      },
    },
    opts = {
      select = {
        lookahead = true,
        selection_modes = {
          ["@parameter.outer"] = "v",
          ["@function.outer"] = "V",
          ["@class.outer"] = "<c-v>",
        },
      },
      move = {
        set_jumps = true,
      },
    },
  },
}
