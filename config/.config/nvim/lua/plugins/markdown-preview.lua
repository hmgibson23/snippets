return {
  "selimacerbas/markdown-preview.nvim",
  ft = { "markdown", "mermaid" },
  cmd = { "MarkdownPreview", "MarkdownPreviewRefresh", "MarkdownPreviewStop" },
  dependencies = { "selimacerbas/live-server.nvim" },
  keys = {
    { "<leader>mpp", "<cmd>MarkdownPreview<cr>", desc = "Start Markdown preview", ft = { "markdown", "mermaid" } },
    { "<leader>mpr", "<cmd>MarkdownPreviewRefresh<cr>", desc = "Refresh Markdown preview", ft = { "markdown", "mermaid" } },
    { "<leader>mpq", "<cmd>MarkdownPreviewStop<cr>", desc = "Stop Markdown preview", ft = { "markdown", "mermaid" } },
  },
  opts = {
    instance_mode = "takeover",
    port = 0,
    open_browser = true,
    auto_refresh = true,
    auto_refresh_events = { "InsertLeave", "TextChanged", "TextChangedI", "BufWritePost" },
    debounce_ms = 250,
    notify_on_refresh = false,
    mermaid_renderer = "js",
    scroll_sync = true,
    bottom_padding = 0.5,
  },
  config = function(_, opts)
    require("markdown_preview").setup(opts)
  end,
}
