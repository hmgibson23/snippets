-- Pi coding agent
-- No plugin to download; just runs config to register keymaps for lua/pi/init.lua.
return {
  dir = vim.fn.stdpath("config"),
  name = "pi-local",
  lazy = false,
  config = function()
    local pi = require("pi")

    vim.keymap.set("n", "<leader>apt", function()
      pi.toggle()
    end, { desc = "Toggle pi" })

    vim.keymap.set("n", "<leader>apf", function()
      pi.focus()
    end, { desc = "Focus pi" })

    vim.keymap.set("n", "<leader>apl", function()
      pi.login()
    end, { desc = "Login (saml2aws)" })
  end,
}
