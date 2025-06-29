local wezterm = require "wezterm"
local act     = wezterm.action
local config  = wezterm.config_builder()

config.audible_bell = "SystemBeep"
config.hide_tab_bar_if_only_one_tab = true
config.font = wezterm.font_with_fallback {
    "CaskaydiaMono Nerd Font Mono",
    -- "UbuntuMono Nerd Font",
    {family = "HarmonyOS Sans SC", scale = 1.1},
}
config.initial_cols = 105
config.initial_rows = 35
config.font_size = 13.0
if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
    config.default_prog = { 'C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe' }
else
    config.window_decorations = "NONE"
end
config.window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
}

-- Colors {{{
config.color_scheme = "One Dark (Gogh)"
-- local myScheme = wezterm.get_builtin_color_schemes()['One Dark (Gogh)']
-- wezterm.log_info(wezterm.to_string(myScheme))
config.colors = {}
config.colors.background = "#1f2430"
config.colors.cursor_bg = "white"
config.colors.cursor_border = "white"
config.colors.cursor_fg = "black"
config.colors.selection_bg = "#434C5E"
config.colors.split = "#4C566A"
config.colors.compose_cursor = "orange"
config.colors.copy_mode_active_highlight_bg = { Color = '#ED427C' }
config.colors.copy_mode_active_highlight_fg = { AnsiColor = 'White' }
config.colors.copy_mode_inactive_highlight_bg = { Color = '#52ad70' }
config.colors.copy_mode_inactive_highlight_fg = { AnsiColor = 'White' }
config.colors.quick_select_label_bg = { Color = '#ED427C' }
config.colors.quick_select_label_fg = { Color = '#ffffff' }
config.colors.quick_select_match_bg = { Color = '#88C0D0' }
config.colors.quick_select_match_fg = { Color = '#ffffff' }
-- }}}
-- Tab {{{
config.use_fancy_tab_bar = true
config.show_tabs_in_tab_bar = true
config.show_new_tab_button_in_tab_bar = true
config.window_frame = {
    -- The font used in the tab bar.
    -- Roboto Bold is the default; this font is bundled
    -- with wezterm.
    -- Whatever font is selected here, it will have the
    -- main font setting appended to it to pick up any
    -- fallback fonts you may have used there.
    font = wezterm.font_with_fallback {
        { family = 'UbuntuMono', weight = 'Bold' },
        -- "UbuntuMono Nerd Font",
        -- {family = "Source Han Serif CN"},
        {family = "HarmonyOS_Sans_SC", scale = 1.0},
    },

    -- The size of the font in the tab bar.
    -- Default to 10.0 on Windows but 12.0 on other systems
    font_size = 10.0,

    -- The overall background color of the tab bar when
    -- the window is focused
    active_titlebar_bg = '#1f2430',

    -- The overall background color of the tab bar when
    -- the window is not focused
    inactive_titlebar_bg = '#1f2430',
}
config.colors.tab_bar = {
    -- The color of the strip that goes along the top of the window
    -- (does not apply when fancy tab bar is in use)
    background = '#1f2430',

    -- The active tab is the one that has focus in the window
    active_tab = {
        bg_color = '#1f2430',
        fg_color = '#D8DEE9',

        -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
        -- label shown for this tab.
        -- The default is "Normal"
        intensity = 'Bold',

        -- Specify whether you want "None", "Single" or "Double" underline for
        -- label shown for this tab.
        -- The default is "None"
        underline = 'None',

        -- Specify whether you want the text to be italic (true) or not (false)
        -- for this tab.  The default is false.
        italic = false,

        -- Specify whether you want the text to be rendered with strikethrough (true)
        -- or not for this tab.  The default is false.
        strikethrough = false,
    },

    -- Inactive tabs are the tabs that do not have focus
    inactive_tab = {
        bg_color = '#1f2430',
        fg_color = '#3f4656',

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `inactive_tab`.
    },

    -- You can configure some alternate styling when the mouse pointer
    -- moves over inactive tabs
    inactive_tab_hover = {
        bg_color = '#434C5E',
        fg_color = '#D8DEE9',
        italic = false,

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `inactive_tab_hover`.
    },

    -- The new tab button that let you create new tabs
    new_tab = {
        bg_color = '#1f2430',
        fg_color = '#434C5E',

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `new_tab`.
    },

    -- You can configure some alternate styling when the mouse pointer
    -- moves over the new tab button
    new_tab_hover = {
        bg_color = '#434C5E',
        fg_color = '#D8DEE9',
        italic = false,

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `new_tab_hover`.
    },
}
-- }}}
-- Key bindings {{{
config.disable_default_key_bindings = true
config.keys = {
    -- Tab
    { key = 'n', mods = 'SHIFT|CTRL', action = act.SpawnWindow },
    { key = 't', mods = 'SHIFT|CTRL', action = act.SpawnTab 'CurrentPaneDomain' },
    { key = 'w', mods = 'SHIFT|CTRL', action = act.CloseCurrentTab{ confirm = true } },
    { key = 'Tab',      mods = 'CTRL',       action = act.ActivateTabRelative(1) },
    { key = 'PageDown', mods = 'CTRL',       action = act.ActivateTabRelative(1) },
    { key = 'Tab',      mods = 'SHIFT|CTRL', action = act.ActivateTabRelative(-1) },
    { key = 'PageUp',   mods = 'CTRL',       action = act.ActivateTabRelative(-1) },
    { key = 'PageUp',   mods = 'ALT',        action = act.MoveTabRelative(-1) },
    { key = 'PageDown', mods = 'ALT',        action = act.MoveTabRelative(1) },
    { key = '!', mods = 'SHIFT|CTRL', action = act.ActivateTab(0) },
    { key = '@', mods = 'SHIFT|CTRL', action = act.ActivateTab(1) },
    { key = '#', mods = 'SHIFT|CTRL', action = act.ActivateTab(2) },
    { key = '$', mods = 'SHIFT|CTRL', action = act.ActivateTab(3) },
    { key = '%', mods = 'SHIFT|CTRL', action = act.ActivateTab(4) },
    { key = '^', mods = 'SHIFT|CTRL', action = act.ActivateTab(5) },
    { key = '&', mods = 'SHIFT|CTRL', action = act.ActivateTab(6) },
    { key = '*', mods = 'SHIFT|CTRL', action = act.ActivateTab(7) },
    { key = '(', mods = 'SHIFT|CTRL', action = act.ActivateTab(8) },
    { key = ')', mods = 'SHIFT|CTRL', action = act.ActivateTab(9) },

    -- Pane
    { key = 'z', mods = 'SHIFT|CTRL', action = act.TogglePaneZoomState },
    { key = 's', mods = 'SHIFT|ALT|CTRL', action = act.SplitVertical{   domain = 'CurrentPaneDomain' } },
    { key = 'v', mods = 'SHIFT|ALT|CTRL', action = act.SplitHorizontal{ domain = 'CurrentPaneDomain' } },
    { key = 'h',          mods = 'SHIFT|ALT|CTRL', action = act.ActivatePaneDirection 'Left'  },
    { key = 'l',          mods = 'SHIFT|ALT|CTRL', action = act.ActivatePaneDirection 'Right' },
    { key = 'k',          mods = 'SHIFT|ALT|CTRL', action = act.ActivatePaneDirection 'Up'    },
    { key = 'j',          mods = 'SHIFT|ALT|CTRL', action = act.ActivatePaneDirection 'Down'  },
    { key = 'LeftArrow',  mods = 'SHIFT|ALT|CTRL', action = act.ActivatePaneDirection 'Left'  },
    { key = 'RightArrow', mods = 'SHIFT|ALT|CTRL', action = act.ActivatePaneDirection 'Right' },
    { key = 'UpArrow',    mods = 'SHIFT|ALT|CTRL', action = act.ActivatePaneDirection 'Up'    },
    { key = 'DownArrow',  mods = 'SHIFT|ALT|CTRL', action = act.ActivatePaneDirection 'Down'  },
    { key = 'LeftArrow',  mods = 'ALT|CTRL', action = act.AdjustPaneSize{ 'Left',  1 } },
    { key = 'RightArrow', mods = 'ALT|CTRL', action = act.AdjustPaneSize{ 'Right', 1 } },
    { key = 'UpArrow',    mods = 'ALT|CTRL', action = act.AdjustPaneSize{ 'Up',    1 } },
    { key = 'DownArrow',  mods = 'ALT|CTRL', action = act.AdjustPaneSize{ 'Down',  1 } },

    -- Navigation
    { key = 'Enter', mods = 'ALT', action = act.ToggleFullScreen },
    { key = 'PageUp',   mods = 'SHIFT|CTRL', action = act.ScrollByPage(-1) },
    { key = 'PageDown', mods = 'SHIFT|CTRL', action = act.ScrollByPage(1) },
    { key = 'e',        mods = 'SHIFT|CTRL', action = act.ScrollByPage(-1) },
    { key = 'd',        mods = 'SHIFT|CTRL', action = act.ScrollByPage(1) },

    -- Font Size
    { key = '=', mods = 'CTRL', action = act.IncreaseFontSize },
    { key = '0', mods = 'CTRL', action = act.ResetFontSize },
    { key = '-', mods = 'CTRL', action = act.DecreaseFontSize },

    -- Copy & Paste
    { key = 'c',      mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
    { key = 'Insert', mods = 'CTRL',       action = act.CopyTo 'PrimarySelection' },
    { key = 'Copy',   mods = 'NONE',       action = act.CopyTo 'Clipboard' },
    { key = 'v',      mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
    { key = 'Insert', mods = 'SHIFT',      action = act.PasteFrom 'PrimarySelection' },
    { key = 'Paste',  mods = 'NONE',       action = act.PasteFrom 'Clipboard' },

    -- Activate Mode
    { key = 'phys:Space', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },
    { key = 'u', mods = 'SHIFT|CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
    { key = 'l', mods = 'SHIFT|CTRL', action = act.ShowDebugOverlay },
    { key = 'p', mods = 'SHIFT|CTRL', action = act.ActivateCommandPalette },
    { key = 'f', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
    { key = '?', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
    { key = 'k', mods = 'SHIFT|CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
    { key = 's', mods = 'SHIFT|CTRL', action = act.QuickSelect },

    { key = 'm', mods = 'SHIFT|CTRL', action = act.Hide },
    { key = 'r', mods = 'SHIFT|CTRL', action = act.ReloadConfiguration },
}
config.key_tables = {
    copy_mode = {
        { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
        { key = 'a',      mods = 'NONE', action = act.CopyMode 'Close' },
        { key = 'i',      mods = 'NONE', action = act.CopyMode 'Close' },
        { key = 'q',      mods = 'NONE', action = act.CopyMode 'Close' },

        { key = 'v', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
        { key = 'V', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Line' } },
        { key = 'v', mods = 'ALT', action = act.CopyMode{ SetSelectionMode =  'Block' } },
        { key = 'o', mods = 'NONE', action  = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
        { key = 'y', mods = 'NONE', action = act.Multiple{ { CopyTo =  'ClipboardAndPrimarySelection' }, { CopyMode =  'Close' } } },

        -- Jumping {{{
        { key = ',', mods = 'NONE', action  = act.CopyMode 'JumpReverse' },
        { key = ';', mods = 'NONE', action  = act.CopyMode 'JumpAgain' },
        { key = 'f', mods = 'NONE', action  = act.CopyMode{ JumpForward  = { prev_char = false } } },
        { key = 'F', mods = 'NONE', action  = act.CopyMode{ JumpBackward = { prev_char = false } } },
        { key = 't', mods = 'NONE', action = act.CopyMode{ JumpForward = { prev_char = true } } },
        { key = 'T', mods = 'NONE', action  = act.CopyMode{ JumpBackward = { prev_char = true } } },
        -- }}}

        -- Navigation {{{
        { key = 'h', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
        { key = 'j', mods = 'NONE', action = act.CopyMode 'MoveDown' },
        { key = 'k', mods = 'NONE', action = act.CopyMode 'MoveUp' },
        { key = 'l', mods = 'NONE', action = act.CopyMode 'MoveRight' },
        { key = 'LeftArrow',  mods = 'NONE', action = act.CopyMode 'MoveLeft' },
        { key = 'UpArrow',    mods = 'NONE', action = act.CopyMode 'MoveUp' },
        { key = 'RightArrow', mods = 'NONE', action = act.CopyMode 'MoveRight' },
        { key = 'DownArrow',  mods = 'NONE', action = act.CopyMode 'MoveDown' },
        { key = 'w', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
        { key = 'e', mods = 'NONE', action = act.CopyMode 'MoveForwardWordEnd' },
        { key = 'b', mods = 'NONE', action = act.CopyMode 'MoveBackwardWord' },
        { key = 'RightArrow', mods = 'CTRL', action = act.CopyMode 'MoveForwardWord' },
        { key = 'LeftArrow',  mods = 'CTRL', action = act.CopyMode 'MoveBackwardWord' },
        { key = 'End',   mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
        { key = '$',     mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
        { key = 'L',     mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
        { key = 'Home',  mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
        { key = '0',     mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
        { key = 'H',     mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
        { key = '^',     mods = 'NONE', action = act.CopyMode 'MoveToStartOfLineContent' },
        { key = 'Enter', mods = 'NONE', action = act.CopyMode 'MoveToStartOfNextLine' },
        { key = 'd', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (0.5)  } },
        { key = 'e', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (-0.5) } },
        { key = 'PageUp',   mods = 'NONE', action = act.CopyMode 'PageUp' },
        { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'PageDown' },
        { key = 'f',        mods = 'CTRL', action = act.CopyMode 'PageDown' },
        { key = 'b',        mods = 'CTRL', action = act.CopyMode 'PageUp' },
        { key = 'd',        mods = 'ALT',  action = act.CopyMode 'PageDown' },
        { key = 'e',        mods = 'ALT',  action = act.CopyMode 'PageUp' },
        { key = 'g', mods = 'NONE',       action = act.CopyMode 'MoveToScrollbackTop' },
        { key = 'G', mods = 'NONE',       action = act.CopyMode 'MoveToScrollbackBottom' },
        { key = 'g', mods = 'CTRL',       action = act.CopyMode 'MoveToViewportTop' },
        { key = 'g', mods = 'CTRL|SHIFT', action = act.CopyMode 'MoveToViewportBottom' },
        -- { key = 'M', mods = 'NONE', action = act.CopyMode 'MoveToViewportMiddle' },
        -- { key = 'M', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportMiddle' },
        -- }}}
    },

    search_mode = {
        { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
        { key = 'c',      mods = 'CTRL', action = act.CopyMode 'Close' },
        { key = 'r', mods = 'CTRL', action = act.CopyMode 'CycleMatchType' },
        { key = 'h', mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
        { key = 'n',         mods = 'CTRL',  action = act.CopyMode 'NextMatch' },
        { key = 'Enter',     mods = 'NONE',  action = act.CopyMode 'NextMatch' },
        { key = 'DownArrow', mods = 'NONE',  action = act.CopyMode 'NextMatch' },
        { key = 'p',         mods = 'CTRL',  action = act.CopyMode 'PriorMatch' },
        { key = 'Enter',     mods = 'SHIFT', action = act.CopyMode 'PriorMatch' },
        { key = 'UpArrow',   mods = 'NONE',  action = act.CopyMode 'PriorMatch' },
        { key = 'PageUp',   mods = 'NONE', action = act.CopyMode 'PriorMatchPage' },
        { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'NextMatchPage' },
    },
}
-- }}}

return config
