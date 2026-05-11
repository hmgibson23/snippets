local function harpoon_list()
  return require("harpoon"):list()
end

return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  keys = {
    {
      "<leader>ha",
      function()
        harpoon_list():add()
      end,
      desc = "Harpoon Add File",
    },
    {
      "<C-e>",
      function()
        local harpoon = require("harpoon")
        harpoon.ui:toggle_quick_menu(harpoon:list())
      end,
      desc = "Harpoon Menu",
    },
    {
      "<C-h>",
      function()
        harpoon_list():select(1)
      end,
      desc = "Harpoon File 1",
    },
    {
      "<C-t>",
      function()
        harpoon_list():select(2)
      end,
      desc = "Harpoon File 2",
    },
    {
      "<C-n>",
      function()
        harpoon_list():select(3)
      end,
      desc = "Harpoon File 3",
    },
    {
      "<C-s>",
      function()
        harpoon_list():select(4)
      end,
      desc = "Harpoon File 4",
    },
    {
      "<C-S-P>",
      function()
        harpoon_list():prev()
      end,
      desc = "Harpoon Previous",
    },
    {
      "<C-S-N>",
      function()
        harpoon_list():next()
      end,
      desc = "Harpoon Next",
    },
  },
  config = function()
    require("harpoon"):setup()
  end,
}
