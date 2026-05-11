local M = {}

function M.run(title, items)
  vim.ui.select(items, {
    prompt = title,
    format_item = function(item)
      return item.label
    end,
  }, function(choice)
    if not choice then
      return
    end
    choice.action()
  end)
end

return M
