describe("workflow-local lazy spec", function()
  local function contains(list, value)
    for _, item in ipairs(list or {}) do
      if item == value then
        return true
      end
    end
    return false
  end

  local function has_key(keys, lhs)
    for _, key in ipairs(keys or {}) do
      if key[1] == lhs then
        return true, key
      end
    end
    return false
  end

  it("declares command triggers for health and diagnostic suppression commands", function()
    local plugin = require("plugins.workflows")

    assert.is_true(contains(plugin.cmd, "ConfigHealth"))
    assert.is_true(contains(plugin.cmd, "ConfigProfile"))
    assert.is_true(contains(plugin.cmd, "ConfigReload"))
    assert.is_true(contains(plugin.cmd, "SuppressDiagnostic"))
    assert.is_true(contains(plugin.cmd, "SuppressDiagnosticAbove"))
    assert.is_true(contains(plugin.cmd, "SuppressDiagnosticFile"))
    assert.is_true(contains(plugin.cmd, "UnsuppressDiagnostic"))
    assert.is_true(contains(plugin.cmd, "PiAsk"))
    assert.is_true(contains(plugin.cmd, "PiAskSelection"))
    assert.is_true(contains(plugin.cmd, "PiReviewChanges"))
  end)

  it("declares suppression keys without using diagnostic summary on suppress inline", function()
    local plugin = require("plugins.workflows")

    local has_inline, inline = has_key(plugin.keys, "<leader>lxs")
    assert.is_true(has_inline)
    assert.are_not.equal("Diagnostic summary", inline.desc)

    assert.is_true(has_key(plugin.keys, "<leader>lxS"))
    assert.is_true(has_key(plugin.keys, "<leader>lxf"))
    assert.is_true(has_key(plugin.keys, "<leader>lxu"))
    assert.is_true(has_key(plugin.keys, "<leader>lxi"))
    assert.is_true(has_key(plugin.keys, "<leader>lxl"))
    assert.is_true(has_key(plugin.keys, "<leader>lC"))
  end)

  it("exposes Pi ask helpers from the local pi module", function()
    local pi = require("pi")

    assert.is_function(pi.ask)
    assert.is_function(pi.ask_selection)
    assert.is_function(pi.open)
  end)

  it("parses only tracked git changes for review", function()
    local ai = require("workflows.ai")

    local items = ai.parse_git_name_status({
      "M\tlua/pi/init.lua",
      "A\tlua/new_tracked.lua",
      "R100\tlua/old.lua\tlua/new.lua",
      "D\tlua/deleted.lua",
    }, "/repo", function(file)
      return "diff for " .. file
    end)

    assert.are.equal(4, #items)
    assert.are.equal("lua/pi/init.lua", items[1].file)
    assert.are.equal("lua/new.lua", items[3].file)
    assert.are.equal("R lua/old.lua -> lua/new.lua", items[3].text)
    assert.are.equal("diff for lua/pi/init.lua", items[1].diff)
  end)

  it("opens a persistent Snacks sidebar from tracked changed-file items", function()
    local ai = require("workflows.ai")
    local original_snacks = _G.Snacks
    local called

    _G.Snacks = {
      picker = setmetatable({}, {
        __call = function(_, opts)
          called = opts
        end,
      }),
    }

    ai.open_review_picker({ { file = "lua/pi/init.lua", text = "M lua/pi/init.lua", diff = "diff" } }, "Tracked changes")

    _G.Snacks = original_snacks

    assert.is_table(called)
    assert.matches("C: commits", called.title)
    assert.are.equal("diff", called.preview)
    assert.are.equal("file", called.format)
    assert.are.equal("list", called.focus)
    assert.are.equal(false, called.auto_close)
    assert.are.equal(false, called.layout.preview)
    assert.are.equal(false, called.jump.close)
    assert.are.equal(true, called.jump.reuse_win)
    assert.are.equal("sidebar", called.layout.preset)
    assert.are.equal("left", called.layout.layout.position)
    assert.is_function(called.actions.pick_review_commit)
    assert.are.equal("pick_review_commit", called.win.input.keys.C[1])
    assert.are.equal("Pick commit to review", called.win.input.keys.C.desc)
  end)

  it("falls back to latest commit when working tree has no tracked changes", function()
    local ai = require("workflows.ai")
    local original_changed_file_items = ai.changed_file_items
    local original_open_review_picker = ai.open_review_picker
    local original_notify = vim.notify
    local calls = {}
    local opened

    ai.changed_file_items = function(commit)
      calls[#calls + 1] = commit or "working-tree"
      if commit == nil then
        return {}
      end
      return { { file = "lua/pi/init.lua", text = "M lua/pi/init.lua", diff = "diff" } }
    end
    ai.open_review_picker = function(items, title)
      opened = { items = items, title = title }
    end
    vim.notify = function() end

    ai.review_changes()

    ai.changed_file_items = original_changed_file_items
    ai.open_review_picker = original_open_review_picker
    vim.notify = original_notify

    assert.same({ "working-tree", "HEAD" }, calls)
    assert.are.equal("Latest commit", opened.title)
    assert.are.equal("lua/pi/init.lua", opened.items[1].file)
  end)

  it("allows inline arguments for Pi ask commands", function()
    require("workflows").setup()

    local commands = vim.api.nvim_get_commands({})
    assert.are.equal("*", commands.PiAsk.nargs)
    assert.are.equal("*", commands.PiAskSelection.nargs)
    assert.are.equal("?", commands.PiReviewChanges.nargs)
    assert.is_true(commands.PiReviewChanges.bang)
  end)
end)
