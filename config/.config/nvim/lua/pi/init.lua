-- Pi coding agent integration
-- Spawns `pi` in a vertical split terminal buffer.
-- Toggling re-uses the same buffer so the session is preserved.

local M = {}

-- State
local state = {
  buf = nil, -- terminal buffer number
  win = nil, -- window handle (nil when hidden)
}

-- Returns true if the buffer is still valid and the terminal job is alive
local function buf_valid()
  if state.buf == nil or not vim.api.nvim_buf_is_valid(state.buf) then
    return false
  end
  -- Also check the terminal job hasn't exited
  return vim.bo[state.buf].buftype == "terminal"
end

-- Returns true if the window is currently visible
local function win_visible()
  return state.win ~= nil and vim.api.nvim_win_is_valid(state.win)
end

-- Open a vertical split on the right and show the pi terminal buffer.
-- If the buffer doesn't exist yet, create it and start `pi`.
local function open()
  local saved_win = vim.api.nvim_get_current_win()

  if not buf_valid() then
    -- Create a new empty buffer for the terminal
    state.buf = vim.api.nvim_create_buf(false, false)

    -- Open a vertical split showing that buffer
    vim.cmd("vsplit")
    state.win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(state.win, state.buf)

    -- Start pi inside it (-c = continue last session)
    vim.fn.termopen("pi -c", {
      cwd = vim.fn.getcwd(),
      on_exit = function()
        -- Invalidate state when pi exits so next toggle spawns fresh
        state.buf = nil
        state.win = nil
      end,
    })

    vim.api.nvim_set_current_win(state.win)
    vim.cmd("startinsert")
    return
  end

  -- Buffer exists but window is not visible – open a new split showing it
  vim.cmd("vsplit")
  state.win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(state.win, state.buf)
  vim.cmd("startinsert")
end

-- Hide the pi window (without killing the process / buffer)
local function hide()
  if win_visible() then
    vim.api.nvim_win_close(state.win, false)
    state.win = nil
  end
end

-- Toggle: open if hidden, hide if visible
function M.toggle()
  if win_visible() then
    hide()
  else
    open()
  end
end

-- Focus the pi window (open it if not visible, then switch to it)
function M.focus()
  if not win_visible() then
    open()
  else
    vim.api.nvim_set_current_win(state.win)
    vim.cmd("startinsert")
  end
end

-- Run `saml2aws login` in a floating terminal, then parse the resulting
-- credentials and inject them into Neovim's environment so that any child
-- process (including the pi terminal) inherits them.
function M.login()
  -- Temp file that the shell script will write KEY=VALUE pairs into
  local creds_file = vim.fn.tempname()

  -- Shell script:
  --   1. Run saml2aws login (interactive – Okta + MFA)
  --   2. On success, emit KEY=VALUE creds into the temp file
  --   3. Exit so TermClose fires
  local script = table.concat({
    "saml2aws login --quiet 2>&1",
    "if [ $? -eq 0 ]; then",
    "  saml2aws script --shell=env > " .. vim.fn.shellescape(creds_file),
    "else",
    "  echo '__SAML2AWS_FAILED__' > " .. vim.fn.shellescape(creds_file),
    "fi",
  }, "\n")

  -- Float dimensions
  local width  = math.floor(vim.o.columns * 0.7)
  local height = math.floor(vim.o.lines   * 0.6)
  local row    = math.floor((vim.o.lines   - height) / 2)
  local col    = math.floor((vim.o.columns - width)  / 2)

  -- Create a scratch buffer for the float
  local float_buf = vim.api.nvim_create_buf(false, true)

  local float_win = vim.api.nvim_open_win(float_buf, true, {
    relative  = "editor",
    width     = width,
    height    = height,
    row       = row,
    col       = col,
    style     = "minimal",
    border    = "rounded",
    title     = " saml2aws login ",
    title_pos = "center",
  })

  -- When the terminal job exits, read the creds file and inject into vim.env
  vim.api.nvim_create_autocmd("TermClose", {
    buffer   = float_buf,
    once     = true,
    callback = function()
      if vim.api.nvim_win_is_valid(float_win) then
        vim.api.nvim_win_close(float_win, true)
      end

      vim.defer_fn(function()
        local f = io.open(creds_file, "r")
        if not f then
          vim.notify("[pi] saml2aws: could not read credentials file", vim.log.levels.ERROR)
          return
        end

        local content = f:read("*a")
        f:close()
        os.remove(creds_file)

        if content:find("__SAML2AWS_FAILED__") then
          vim.notify("[pi] saml2aws login failed", vim.log.levels.ERROR)
          return
        end

        local injected = 0
        for line in content:gmatch("[^\n]+") do
          local key, value = line:match("^([A-Z_][A-Z0-9_]*)=(.*)$")
          if key then
            vim.env[key] = value
            injected = injected + 1
          end
        end

        if injected > 0 then
          vim.notify(string.format("[pi] AWS credentials injected (%d vars)", injected), vim.log.levels.INFO)
        else
          vim.notify("[pi] saml2aws: no credentials found in output", vim.log.levels.WARN)
        end
      end, 100)
    end,
  })

  vim.fn.termopen({ "bash", "-c", script }, { cwd = vim.fn.getcwd() })
  vim.cmd("startinsert")
end

return M
