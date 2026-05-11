local wezterm = require("wezterm")
local act = wezterm.action

-- ========================
-- Catppuccin Macchiato Powerline WezTerm
-- ========================
local scheme = wezterm.color.get_builtin_schemes()["Catppuccin Macchiato"]
local nf = wezterm.nerdfonts

local palette = {
  rosewater = "#f4dbd6",
  flamingo = "#f0c6c6",
  pink = "#f5bde6",
  mauve = "#c6a0f6",
  red = "#ed8796",
  maroon = "#ee99a0",
  peach = "#f5a97f",
  yellow = "#eed49f",
  green = "#a6da95",
  teal = "#8bd5ca",
  sky = "#91d7e3",
  sapphire = "#7dc4e4",
  blue = "#8aadf4",
  lavender = "#b7bdf8",
  text = "#cad3f5",
  subtext1 = "#b8c0e0",
  subtext0 = "#a5adcb",
  overlay2 = "#939ab7",
  overlay1 = "#8087a2",
  overlay0 = "#6e738d",
  surface2 = "#5b6078",
  surface1 = "#494d64",
  surface0 = "#363a4f",
  base = "#24273a",
  mantle = "#1e2030",
  crust = "#181926",
}

local function icon(name, fallback)
  return nf[name] or fallback
end

-- Use literal Powerline round-cap glyphs for the tab edges.  Some older
-- WezTerm/Nerd Font combinations don't expose the `ple_*half_circle*` names
-- reliably through wezterm.nerdfonts, while these codepoints are stable in
-- Nerd Font/Symbols Nerd Font.
local icons = {
  left = "",
  right = "",
  sep = "",
  terminal = icon("cod_terminal", ""),
  shell = icon("dev_terminal", ""),
  fish = icon("md_fish", "󰈺"),
  vim = icon("dev_vim", ""),
  git = icon("dev_git", ""),
  docker = icon("linux_docker", ""),
  k8s = icon("md_kubernetes", "󱃾"),
  ssh = icon("md_remote_desktop", "󰢹"),
  node = icon("dev_nodejs_small", ""),
  python = icon("dev_python", ""),
  rust = icon("dev_rust", ""),
  go = icon("seti_go", ""),
  lua = icon("seti_lua", ""),
  package = icon("cod_package", "󰏗"),
  cwd = icon("oct_file_directory", ""),
  clock = icon("md_clock_outline", "󰥔"),
  battery = icon("md_battery", "󰁹"),
  branch = icon("oct_git_branch", ""),
  zoom = icon("cod_screen_full", "󰊓"),
}

local process_icon_color = {
  nvim = { icons.vim, palette.green },
  vim = { icons.vim, palette.green },
  lazygit = { icons.git, palette.peach },
  git = { icons.git, palette.peach },
  docker = { icons.docker, palette.sapphire },
  podman = { icons.docker, palette.sapphire },
  kubectl = { icons.k8s, palette.blue },
  kind = { icons.k8s, palette.blue },
  ssh = { icons.ssh, palette.mauve },
  node = { icons.node, palette.green },
  npm = { icons.node, palette.red },
  yarn = { icons.package, palette.blue },
  pnpm = { icons.package, palette.yellow },
  python = { icons.python, palette.yellow },
  ipython = { icons.python, palette.yellow },
  cargo = { icons.rust, palette.peach },
  rustc = { icons.rust, palette.peach },
  go = { icons.go, palette.sky },
  lua = { icons.lua, palette.blue },
  fish = { icons.fish, palette.teal },
  zsh = { icons.shell, palette.teal },
  bash = { icons.shell, palette.teal },
}

local function basename(path)
  if not path or path == "" then
    return "wezterm"
  end
  path = path:gsub("\\", "/")
  return path:match("([^/]+)$") or path
end

local function clean_title(title)
  title = title or "wezterm"
  title = title:gsub("^%s+", ""):gsub("%s+$", "")
  title = title:gsub("^%d+:%s*", "")
  return title ~= "" and title or "wezterm"
end

local function truncate_right(str, max)
  if #str <= max then
    return str
  end
  if max <= 1 then
    return "…"
  end
  return str:sub(1, max - 1) .. "…"
end

local function pane_program(pane)
  return basename(pane.foreground_process_name):lower()
end

local function tab_icon_and_color(pane)
  local program = pane_program(pane)
  for key, value in pairs(process_icon_color) do
    if program:find(key, 1, true) then
      return value[1], value[2]
    end
  end
  return icons.terminal, palette.lavender
end

-- ========================
-- Base Config
-- ========================
local config = {
  check_for_updates = false,
  window_close_confirmation = "NeverPrompt",
  enable_wayland = false,
  default_cwd = os.getenv("HOME"),
  default_prog = { "/opt/homebrew/bin/fish" },
  scrollback_lines = 10000,
  enable_scroll_bar = true,
  enable_csi_u_key_encoding = true,
  exit_behavior = "Close",
  audible_bell = "Disabled",
  automatically_reload_config = true,

  -- Keep shortcuts independent of the active keyboard layout where useful.
  -- This makes Colemak/Dvorak/QWERTY muscle-memory bindings coexist.
  key_map_preference = "Mapped",

  color_scheme = "Catppuccin Macchiato",
  colors = {
    tab_bar = {
      background = palette.crust,
    },
  },

  font = wezterm.font_with_fallback({
    "MesloLGL Nerd Font Mono",
    "Symbols Nerd Font Mono",
    "Apple Color Emoji",
  }),
  font_size = 14.0,
  line_height = 1.04,
  freetype_load_target = "Light",

  window_frame = {
    font = wezterm.font_with_fallback({
      { family = "MesloLGL Nerd Font Mono", weight = "Bold" },
      "Symbols Nerd Font Mono",
      "Apple Color Emoji",
    }),
    font_size = 13.0,
    active_titlebar_bg = palette.crust,
    inactive_titlebar_bg = palette.crust,
    border_left_width = "0cell",
    border_right_width = "0cell",
    border_bottom_height = "0cell",
    border_top_height = "0.20cell",
    border_left_color = palette.base,
    border_right_color = palette.base,
    border_bottom_color = palette.base,
    border_top_color = palette.mauve,
  },
  window_decorations = "INTEGRATED_BUTTONS|RESIZE",
  window_padding = { left = 8, right = 8, top = 7, bottom = 3 },
  text_background_opacity = 1.0,
  window_background_opacity = 0.96,
  macos_window_background_blur = 18,
  inactive_pane_hsb = { saturation = 0.90, brightness = 0.72 },

  cursor_blink_ease_in = "Constant",
  cursor_blink_ease_out = "Constant",
  cursor_blink_rate = 650,
  default_cursor_style = "BlinkingBlock",
  force_reverse_video_cursor = false,
  hide_mouse_cursor_when_typing = true,

  enable_tab_bar = true,
  hide_tab_bar_if_only_one_tab = false,
  show_new_tab_button_in_tab_bar = false,
  show_tab_index_in_tab_bar = false,
  status_update_interval = 1000,
  tab_bar_at_bottom = false,
  tab_max_width = 36,
  use_fancy_tab_bar = false,

  keys = {
    { key = "Space", mods = "ALT", action = act.ToggleFullScreen },
    { key = "P", mods = "CMD|SHIFT", action = act.ActivateCommandPalette },
    { key = "K", mods = "CMD|SHIFT", action = act.ClearScrollback("ScrollbackAndViewport") },
    { key = "Enter", mods = "CMD|SHIFT", action = act.TogglePaneZoomState },

    -- Tabs: creation, navigation, and iTerm/browser-like reordering.
    { key = "T", mods = "CMD", action = act.SpawnTab("CurrentPaneDomain") },
    { key = "W", mods = "CMD", action = act.CloseCurrentTab({ confirm = false }) },
    { key = "LeftArrow", mods = "CMD|SHIFT", action = act.MoveTabRelative(-1) },
    { key = "RightArrow", mods = "CMD|SHIFT", action = act.MoveTabRelative(1) },
    { key = "[", mods = "CMD|SHIFT", action = act.MoveTabRelative(-1) },
    { key = "]", mods = "CMD|SHIFT", action = act.MoveTabRelative(1) },
    { key = "[", mods = "CMD", action = act.ActivateTabRelative(-1) },
    { key = "]", mods = "CMD", action = act.ActivateTabRelative(1) },

    -- Splits.
    { key = "UpArrow", mods = "CMD", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "DownArrow", mods = "CMD", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "LeftArrow", mods = "CMD", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "RightArrow", mods = "CMD", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "|", mods = "CMD|SHIFT", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "_", mods = "CMD|SHIFT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
    { key = "X", mods = "CMD|SHIFT", action = act.CloseCurrentPane({ confirm = false }) },
    { key = "R", mods = "CMD|SHIFT", action = act.RotatePanes("Clockwise") },
    { key = "P", mods = "CMD|ALT", action = act.PaneSelect({ alphabet = "arstneiohqwfpjluy", mode = "Activate" }) },

    -- Pane navigation: arrows, Vim/QWERTY, and Colemak home-row (h/n/e/i).
    { key = "UpArrow", mods = "ALT", action = act.ActivatePaneDirection("Up") },
    { key = "DownArrow", mods = "ALT", action = act.ActivatePaneDirection("Down") },
    { key = "LeftArrow", mods = "ALT", action = act.ActivatePaneDirection("Left") },
    { key = "RightArrow", mods = "ALT", action = act.ActivatePaneDirection("Right") },
    { key = "h", mods = "ALT", action = act.ActivatePaneDirection("Left") },
    { key = "j", mods = "ALT", action = act.ActivatePaneDirection("Down") },
    { key = "k", mods = "ALT", action = act.ActivatePaneDirection("Up") },
    { key = "l", mods = "ALT", action = act.ActivatePaneDirection("Right") },
    { key = "n", mods = "ALT", action = act.ActivatePaneDirection("Down") },
    { key = "e", mods = "ALT", action = act.ActivatePaneDirection("Up") },
    { key = "i", mods = "ALT", action = act.ActivatePaneDirection("Right") },

    -- Resize panes.
    { key = "LeftArrow", mods = "CMD|ALT", action = act.AdjustPaneSize({ "Left", 3 }) },
    { key = "RightArrow", mods = "CMD|ALT", action = act.AdjustPaneSize({ "Right", 3 }) },
    { key = "UpArrow", mods = "CMD|ALT", action = act.AdjustPaneSize({ "Up", 2 }) },
    { key = "DownArrow", mods = "CMD|ALT", action = act.AdjustPaneSize({ "Down", 2 }) },
    { key = "h", mods = "CMD|ALT", action = act.AdjustPaneSize({ "Left", 3 }) },
    { key = "n", mods = "CMD|ALT", action = act.AdjustPaneSize({ "Down", 2 }) },
    { key = "e", mods = "CMD|ALT", action = act.AdjustPaneSize({ "Up", 2 }) },
    { key = "i", mods = "CMD|ALT", action = act.AdjustPaneSize({ "Right", 3 }) },

    -- Copy / paste and selection helpers.
    { key = "C", mods = "CMD", action = act.CopyTo("Clipboard") },
    { key = "V", mods = "CMD", action = act.PasteFrom("Clipboard") },
    { key = "F", mods = "CMD", action = act.Search("CurrentSelectionOrEmptyString") },
    { key = "U", mods = "CMD|SHIFT", action = act.QuickSelect },
    { key = "Escape", mods = "CMD", action = act.ActivateCopyMode },

    -- Shell/editor cursor movement.
    { key = "LeftArrow", mods = "CTRL", action = act.SendString("\x1bb") },
    { key = "RightArrow", mods = "CTRL", action = act.SendString("\x1bf") },
    { key = "Home", mods = "CTRL", action = act.SendString("\x1b[1~") },
    { key = "End", mods = "CTRL", action = act.SendString("\x1b[4~") },
    { key = "Backspace", mods = "CMD", action = act.SendString("\x1b[w") },
    { key = "Delete", mods = "CMD", action = act.SendString("\x1b\x7f") },
  },
}

for i = 1, 8 do
  table.insert(config.keys, { key = tostring(i), mods = "CMD", action = act.ActivateTab(i - 1) })
end
table.insert(config.keys, { key = "9", mods = "CMD", action = act.ActivateTab(-1) })

-- ========================
-- Right-status: directory, battery, and time
-- ========================
wezterm.on("update-right-status", function(window, pane)
  local cwd_uri = pane.current_working_dir and tostring(pane.current_working_dir) or ""
  local cwd = cwd_uri:gsub("^file://", ""):gsub("%%20", " ")
  local cwd_name = basename(cwd)

  local battery = ""
  for _, b in ipairs(wezterm.battery_info() or {}) do
    battery = string.format(" %.0f%%%%", b.state_of_charge * 100)
    break
  end

  window:set_right_status(wezterm.format({
    { Background = { Color = palette.crust } },
    { Foreground = { Color = palette.surface2 } },
    { Text = " " .. icons.sep .. " " },
    { Foreground = { Color = palette.blue } },
    { Text = icons.cwd .. " " },
    { Foreground = { Color = palette.text } },
    { Text = truncate_right(cwd_name, 28) .. "  " },
    { Foreground = { Color = palette.green } },
    { Text = icons.battery .. battery .. "  " },
    { Foreground = { Color = palette.mauve } },
    { Text = icons.clock .. " " .. wezterm.strftime("%H:%M") .. " " },
  }))
end)

-- ========================
-- Custom Tab Title Formatting
-- ========================
wezterm.on("format-tab-title", function(tab, tabs, panes, cfg, hover, max_width)
  local pane = tab.active_pane
  local glyph, accent = tab_icon_and_color(pane)
  local title = truncate_right(clean_title(pane.title), math.max(8, max_width - 10))
  local index = tostring(tab.tab_index + 1)
  local zoom = tab.active_pane.is_zoomed and (" " .. icons.zoom) or ""
  local has_unseen = false

  for _, p in ipairs(tab.panes) do
    if p.has_unseen_output then
      has_unseen = true
      break
    end
  end

  if tab.is_active then
    local bg = hover and palette.surface1 or accent
    return {
      { Background = { Color = palette.crust } },
      { Foreground = { Color = bg } },
      { Text = icons.left },
      { Background = { Color = bg } },
      { Foreground = { Color = palette.crust } },
      { Text = " " .. index .. " " .. glyph .. " " },
      { Foreground = { Color = palette.base } },
      { Text = title .. zoom .. " " },
      { Background = { Color = palette.crust } },
      { Foreground = { Color = bg } },
      { Text = icons.right .. " " },
    }
  end

  local fg = has_unseen and palette.yellow or palette.overlay1
  local bg = hover and palette.surface1 or palette.surface0
  return {
    { Background = { Color = palette.crust } },
    { Foreground = { Color = bg } },
    { Text = icons.left },
    { Background = { Color = bg } },
    { Foreground = { Color = accent } },
    { Text = " " .. glyph .. " " },
    { Foreground = { Color = fg } },
    { Text = title .. " " },
    { Background = { Color = palette.crust } },
    { Foreground = { Color = bg } },
    { Text = icons.right .. " " },
  }
end)

return config
