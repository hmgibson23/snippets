-- Pi coding agent integration
-- Spawns `pi` in a vertical split terminal buffer.
-- Toggling re-uses the same buffer so the session is preserved.

local M = {}

-- State
local state = {
  buf = nil, -- terminal buffer number
  win = nil, -- window handle (nil when hidden)
  job_id = nil, -- terminal channel/job id
}

local MAX_CONTEXT_BYTES = 24000

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
    state.job_id = vim.fn.termopen("pi -c", {
      cwd = vim.fn.getcwd(),
      on_exit = function()
        -- Invalidate state when pi exits so next toggle spawns fresh
        state.buf = nil
        state.win = nil
        state.job_id = nil
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

-- Open the pi sidecar and keep focus in the original window when possible.
function M.open()
  local saved_win = vim.api.nvim_get_current_win()
  if not win_visible() then
    open()
  end
  if vim.api.nvim_win_is_valid(saved_win) then
    vim.api.nvim_set_current_win(saved_win)
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

local function trim_context(text)
  if #text <= MAX_CONTEXT_BYTES then
    return text
  end

  return text:sub(1, MAX_CONTEXT_BYTES) .. "\n\n...[truncated by pi.nvim context helper]..."
end

local function current_buffer_context()
  local name = vim.api.nvim_buf_get_name(0)
  local filetype = vim.bo.filetype or ""
  local cursor = vim.api.nvim_win_get_cursor(0)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)

  return table.concat({
    "Current Neovim buffer context:",
    "- cwd: " .. vim.fn.getcwd(),
    "- file: " .. (name ~= "" and name or "[No Name]"),
    "- cursor: line " .. cursor[1] .. ", column " .. cursor[2],
    "",
    "```" .. filetype,
    trim_context(table.concat(lines, "\n")),
    "```",
  }, "\n")
end

local function range_context(line1, line2)
  local name = vim.api.nvim_buf_get_name(0)
  local filetype = vim.bo.filetype or ""
  local lines = vim.api.nvim_buf_get_lines(0, line1 - 1, line2, false)

  return table.concat({
    "Selected Neovim context:",
    "- cwd: " .. vim.fn.getcwd(),
    "- file: " .. (name ~= "" and name or "[No Name]"),
    "- lines: " .. line1 .. "-" .. line2,
    "",
    "```" .. filetype,
    trim_context(table.concat(lines, "\n")),
    "```",
  }, "\n")
end

local function send_to_pi(text)
  local had_valid_buffer = buf_valid()
  M.focus()

  local job_id = state.job_id or (state.buf and vim.b[state.buf].terminal_job_id)
  if not job_id then
    vim.notify("[pi] terminal job is not available", vim.log.levels.ERROR)
    return
  end
  state.job_id = job_id

  local delay_ms = had_valid_buffer and 50 or 500
  vim.defer_fn(function()
    if state.job_id then
      -- Bracketed paste prevents multiline context from being submitted line-by-line.
      vim.fn.chansend(state.job_id, "\027[200~" .. text .. "\027[201~\r")
    end
  end, delay_ms)
end

local function ask_with_context(context, question)
  if question and question ~= "" then
    send_to_pi(question .. "\n\n" .. context)
    return
  end

  vim.ui.input({ prompt = "Ask pi: " }, function(input)
    if not input or input == "" then
      return
    end

    send_to_pi(input .. "\n\n" .. context)
  end)
end

function M.ask(question)
  ask_with_context(current_buffer_context(), question)
end

function M.ask_selection(opts)
  opts = opts or {}
  local line1 = opts.line1 or vim.fn.line("'<")
  local line2 = opts.line2 or vim.fn.line("'>")

  if line1 <= 0 or line2 <= 0 then
    line1 = vim.fn.line(".")
    line2 = line1
  end

  if line2 < line1 then
    line1, line2 = line2, line1
  end

  ask_with_context(range_context(line1, line2), opts.question)
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
