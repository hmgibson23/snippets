local registry = require("mason-registry")

local M = {}
local declined = {}
local asked = {}
local pending = {}
local installing = {}
local refresh_running = false

local function finish(callback, ok)
  if callback then
    vim.schedule(function()
      callback(ok)
    end)
  end
end

local function start_pending_installs()
  if refresh_running then
    return
  end

  refresh_running = true
  registry.refresh(function(success)
    refresh_running = false

    for package_name, callbacks in pairs(pending) do
      pending[package_name] = nil
      installing[package_name] = callbacks

      if not success then
        vim.notify("Mason registry refresh failed; cannot install " .. package_name, vim.log.levels.ERROR)
        asked[package_name] = nil
        installing[package_name] = nil
        for _, callback in ipairs(callbacks) do
          finish(callback, false)
        end
      else
        local ok, package = pcall(registry.get_package, package_name)
        if not ok then
          vim.notify("Mason package not found: " .. package_name, vim.log.levels.ERROR)
          asked[package_name] = nil
          installing[package_name] = nil
          for _, callback in ipairs(callbacks) do
            finish(callback, false)
          end
        elseif package:is_installed() then
          installing[package_name] = nil
          for _, callback in ipairs(callbacks) do
            finish(callback, true)
          end
        else
          local install_ok, err = pcall(function()
            package:install({}, function(installed, install_error)
              installing[package_name] = nil
              if not installed then
                asked[package_name] = nil
                vim.notify(
                  ("Mason failed to install %s: %s"):format(package_name, tostring(install_error)),
                  vim.log.levels.ERROR
                )
              end
              for _, callback in ipairs(callbacks) do
                finish(callback, installed)
              end
            end)
          end)
          if not install_ok then
            asked[package_name] = nil
            installing[package_name] = nil
            vim.notify("Mason could not install " .. package_name .. ": " .. tostring(err), vim.log.levels.ERROR)
            for _, callback in ipairs(callbacks) do
              finish(callback, false)
            end
          end
        end
      end
    end
  end)
end

local function request(package_name, callback)
  if registry.is_installed(package_name) then
    finish(callback, true)
    return
  end

  if declined[package_name] then
    finish(callback, false)
    return
  end

  if installing[package_name] then
    table.insert(installing[package_name], callback)
  elseif pending[package_name] then
    table.insert(pending[package_name], callback)
  else
    pending[package_name] = { callback }
  end

  start_pending_installs()
end

---@param packages string[]
---@param reason string
---@param callback fun(success: boolean)
function M.ensure(packages, reason, callback)
  local unique = {}
  local missing = {}

  for _, package_name in ipairs(packages) do
    if package_name and not unique[package_name] and not registry.is_installed(package_name) then
      unique[package_name] = true
      table.insert(missing, package_name)
    end
  end

  if #missing == 0 then
    finish(callback, true)
    return
  end

  local to_ask = {}
  for _, package_name in ipairs(missing) do
    if not asked[package_name] and not declined[package_name] then
      table.insert(to_ask, package_name)
    end
  end

  if #to_ask > 0 then
    local prompt = ("Install Mason package%s for %s?\n\n%s"):format(
      #to_ask == 1 and "" or "s",
      reason,
      table.concat(to_ask, "\n")
    )
    local confirmed = vim.fn.confirm(prompt, "&Yes\n&No", 1) == 1

    for _, package_name in ipairs(to_ask) do
      if confirmed then
        asked[package_name] = true
      else
        declined[package_name] = true
      end
    end
  end

  local remaining = 0
  local success = true
  for _, package_name in ipairs(missing) do
    if declined[package_name] then
      success = false
    else
      remaining = remaining + 1
      request(package_name, function(installed)
        success = success and installed
        remaining = remaining - 1
        if remaining == 0 then
          finish(callback, success)
        end
      end)
    end
  end

  if remaining == 0 then
    finish(callback, success)
  end
end

function M.ensure_specs(specs, reason, callback)
  local packages = {}
  for _, spec in ipairs(specs) do
    if spec.mason then
      table.insert(packages, spec.mason)
    end
  end
  M.ensure(packages, reason, callback)
end

return M
