local select = require("workflows.select")

local M = {}

local function cmd(command)
  return function()
    vim.cmd(command)
  end
end

local function has_command(command)
  return vim.fn.exists(":" .. command) == 2
end

function M.actions()
  local actions = {}

  if has_command("CodeCompanionChat") then
    table.insert(actions, { label = "Chat: CodeCompanion", action = cmd("CodeCompanionChat") })
    table.insert(actions, { label = "Inline ask: CodeCompanion", action = cmd("CodeCompanion") })
    table.insert(actions, { label = "Actions: CodeCompanion", action = cmd("CodeCompanionActions") })
  end

  table.insert(actions, {
    label = "Agent: Pi toggle",
    action = function()
      require("pi").toggle()
    end,
  })
  table.insert(actions, {
    label = "Agent: Pi focus",
    action = function()
      require("pi").focus()
    end,
  })

  table.insert(actions, {
    label = "Agent: OpenCode toggle",
    action = function()
      require("opencode").toggle()
    end,
  })
  table.insert(actions, {
    label = "Agent: OpenCode ask cursor/selection",
    action = function()
      local mode = vim.fn.mode()
      if mode == "v" or mode == "V" or mode == "\22" then
        require("opencode").ask("@selection: ")
      else
        require("opencode").ask("@cursor: ")
      end
    end,
  })

  if has_command("AvanteAsk") then
    table.insert(actions, { label = "Workspace AI: Avante ask", action = cmd("AvanteAsk") })
  end
  if has_command("Aider") then
    table.insert(actions, { label = "Agent: Aider", action = cmd("Aider") })
  end
  if has_command("Copilot") then
    table.insert(actions, { label = "Inline: Copilot panel/status", action = cmd("Copilot status") })
  end

  return actions
end

function M.palette()
  select.run("AI actions", M.actions())
end

local function git(args, cwd)
  local cmd_args = { "git" }
  if cwd then
    vim.list_extend(cmd_args, { "-C", cwd })
  end
  vim.list_extend(cmd_args, args)
  return vim.fn.systemlist(cmd_args)
end

local function git_root()
  local lines = git({ "rev-parse", "--show-toplevel" })
  if vim.v.shell_error == 0 and lines[1] and lines[1] ~= "" then
    return lines[1]
  end
  return vim.fn.getcwd()
end

local function join(lines)
  return table.concat(lines or {}, "\n")
end

function M.parse_git_name_status(lines, cwd, diff_builder)
  local items = {}
  for _, line in ipairs(lines or {}) do
    if line ~= "" then
      local parts = vim.split(line, "\t", { plain = true })
      local status = parts[1] or ""
      local code = status:sub(1, 1)
      local file = (code == "R" or code == "C") and parts[3] or parts[2]
      local old_file = (code == "R" or code == "C") and parts[2] or nil

      if file and file ~= "" then
        local text = code .. " " .. file
        if old_file then
          text = code .. " " .. old_file .. " -> " .. file
        end

        items[#items + 1] = {
          text = text,
          file = file,
          cwd = cwd,
          pos = { 1, 0 },
          diff = diff_builder and diff_builder(file, old_file, code) or nil,
        }
      end
    end
  end
  return items
end

function M.changed_file_items(commit)
  local root = git_root()
  if commit and commit ~= "" then
    local lines = git({ "diff-tree", "--no-commit-id", "--name-status", "-r", "--diff-filter=ACMRTUXB", commit }, root)
    return M.parse_git_name_status(lines, root, function(file)
      return join(git({ "show", "--format=", "--no-color", "--no-ext-diff", commit, "--", file }, root))
    end)
  end

  local lines = git({ "diff", "--name-status", "--diff-filter=ACMRTUXB", "HEAD", "--" }, root)
  return M.parse_git_name_status(lines, root, function(file)
    return join(git({ "diff", "--no-color", "--no-ext-diff", "HEAD", "--", file }, root))
  end)
end

function M.open_review_picker(items, title)
  if not (_G.Snacks and Snacks.picker) then
    local opened = pcall(vim.cmd, "DiffviewOpen HEAD")
    if not opened then
      pcall(vim.cmd, "Neogit")
    end
    vim.notify("Snacks picker unavailable; opened fallback git review UI.", vim.log.levels.WARN)
    return
  end

  Snacks.picker({
    title = (title or "Tracked changes") .. "  (C: commits)",
    items = items,
    format = "file",
    preview = "diff",
    focus = "list",
    enter = true,
    auto_close = false,
    jump = {
      close = false,
      reuse_win = true,
    },
    layout = {
      preset = "sidebar",
      preview = false,
      layout = {
        position = "left",
        width = 42,
      },
    },
    previewers = {
      diff = {
        style = "fancy",
      },
    },
    actions = {
      pick_review_commit = function(picker)
        picker:close()
        M.pick_review_commit()
      end,
    },
    win = {
      input = {
        keys = {
          C = { "pick_review_commit", mode = { "n", "i" }, desc = "Pick commit to review" },
        },
      },
      list = {
        keys = {
          C = { "pick_review_commit", desc = "Pick commit to review" },
        },
      },
    },
  })
  vim.notify("Review changed tracked files on the left; select text in the editor and use <leader>apa, :PiAsk, or :PiAskSelection.", vim.log.levels.INFO)
end

function M.pick_review_commit()
  local root = git_root()
  local lines = git({ "log", "--date=short", "--pretty=format:%h%x09%ad%x09%s", "--max-count=100" }, root)
  local items = {}
  for _, line in ipairs(lines or {}) do
    local commit, date, subject = line:match("^(%S+)%s+([^%s]+)%s+(.+)$")
    if commit then
      items[#items + 1] = {
        text = commit .. "  " .. date .. "  " .. subject,
        commit = commit,
      }
    end
  end

  if _G.Snacks and Snacks.picker then
    Snacks.picker({
      title = "Pick commit to review",
      items = items,
      format = "text",
      focus = "list",
      confirm = function(picker, item)
        picker:close()
        if item then
          M.review_changes(item.commit)
        end
      end,
    })
  end
end

function M.review_changes(commit)
  if commit == "pick" then
    M.pick_review_commit()
    return
  end

  local items = M.changed_file_items(commit)
  local title = commit and ("Commit " .. commit) or "Tracked changes"

  if #items == 0 and not commit then
    items = M.changed_file_items("HEAD")
    title = "Latest commit"
  end

  if #items == 0 then
    vim.notify("No tracked git changes to review.", vim.log.levels.INFO)
    return
  end

  M.open_review_picker(items, title)
end

function M.setup()
  vim.api.nvim_create_user_command("AiPalette", M.palette, { desc = "Unified AI action palette" })
  vim.api.nvim_create_user_command("PiToggle", function() require("pi").toggle() end, { desc = "Toggle pi" })
  vim.api.nvim_create_user_command("PiFocus", function() require("pi").focus() end, { desc = "Focus pi" })
  vim.api.nvim_create_user_command("PiLogin", function() require("pi").login() end, { desc = "Login via saml2aws" })
  vim.api.nvim_create_user_command("PiAsk", function(opts)
    require("pi").ask(opts.args)
  end, { nargs = "*", desc = "Ask pi with current buffer context" })
  vim.api.nvim_create_user_command("PiAskSelection", function(opts)
    require("pi").ask_selection({ line1 = opts.line1, line2 = opts.line2, question = opts.args })
  end, { range = true, nargs = "*", desc = "Ask pi with selected line context" })
  vim.api.nvim_create_user_command("PiReviewChanges", function(opts)
    if opts.bang then
      M.pick_review_commit()
    else
      M.review_changes(opts.args ~= "" and opts.args or nil)
    end
  end, { bang = true, nargs = "?", desc = "Review tracked git changes or a picked commit" })

  vim.keymap.set({ "n", "v" }, "<leader>aP", M.palette, { desc = "AI palette" })
  vim.keymap.set("n", "<leader>apt", function() require("pi").toggle() end, { desc = "Toggle pi" })
  vim.keymap.set("n", "<leader>apf", function() require("pi").focus() end, { desc = "Focus pi" })
  vim.keymap.set("n", "<leader>apl", function() require("pi").login() end, { desc = "Login (saml2aws)" })
  vim.keymap.set("n", "<leader>apa", function() require("pi").ask() end, { desc = "Ask pi" })
  vim.keymap.set("v", "<leader>apa", function() require("pi").ask_selection() end, { desc = "Ask pi about selection" })
  vim.keymap.set("n", "<leader>apr", M.review_changes, { desc = "Review changes with pi" })
end

return M
