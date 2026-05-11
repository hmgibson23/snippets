return {
  dir = vim.fn.stdpath("config"),
  name = "workflow-local",
  lazy = false,
  config = function()
    require("workflows").setup()
  end,
}
