# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config: ConfigAPI = config  # noqa: F821 pylint: disable=E0602,C0103
c: ConfigContainer = c  # noqa: F821 pylint: disable=E0602,C0103

nord = [
    # Polar Night
    '#2e3440',  # Nord0
    '#3b4252',  # Nord1
    '#434c5e',  # Nord2
    '#4c566a',  # Nord3
    # Snow Storm
    '#d8dee9',  # Nord4
    '#e5e9f0',  # Nord5
    '#eceff4',  # Nord6
    # Frost
    '#8fbcbb',  # Nord7
    '#88c0d0',  # Nord8
    '#81a1c1',  # Nord9
    '#5e81ac',  # Nord10
    # Aurora
    '#bf616a',  # Nord11
    '#d08770',  # Nord12
    '#ebcb8b',  # Nord13
    '#a3be8c',  # Nord14
    '#b48ead',  # Nord15
]

# Background color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.bg = nord[0]

# Bottom border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.border.bottom = nord[0]

# Top border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.border.top = nord[0]

# Foreground color of completion widget category headers.
# Type: QtColor
c.colors.completion.category.fg = nord[5]

# Background color of the completion widget for even rows.
# Type: QssColor
c.colors.completion.even.bg = nord[1]

# Background color of the completion widget for odd rows.
# Type: QssColor
c.colors.completion.odd.bg = nord[1]

# Text color of the completion widget.
# Type: QtColor
c.colors.completion.fg = nord[4]

# Background color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.bg = nord[3]

# Bottom border color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.border.bottom = nord[3]

# Top border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.item.selected.border.top = nord[3]

# Foreground color of the matched text in the selected completion item.
# Type: QtColor
c.colors.completion.item.selected.fg = nord[6]

# Foreground color of the matched text in the completion.
# Type: QssColor
c.colors.completion.match.fg = nord[13]

# Color of the scrollbar in completion view
# Type: QssColor
c.colors.completion.scrollbar.bg = nord[1]

# Color of the scrollbar handle in completion view.
# Type: QssColor
c.colors.completion.scrollbar.fg = nord[5]

# Background color of disabled items in the context menu. If set to
# null, the Qt default is used.
# Type: QssColor
c.colors.contextmenu.disabled.bg = None

# Foreground color of disabled items in the context menu. If set to
# null, the Qt default is used.
# Type: QssColor
c.colors.contextmenu.disabled.fg = None

# Background color of the context menu. If set to null, the Qt default
# is used.
# Type: QssColor
c.colors.contextmenu.menu.bg = None

# Foreground color of the context menu. If set to null, the Qt default
# is used.
# Type: QssColor
c.colors.contextmenu.menu.fg = None

# Background color of the context menu's selected item. If set to null,
# the Qt default is used.
# Type: QssColor
c.colors.contextmenu.selected.bg = None

# Foreground color of the context menu's selected item. If set to null,
# the Qt default is used.
# Type: QssColor
c.colors.contextmenu.selected.fg = None

# Background color for the download bar.
# Type: QssColor
c.colors.downloads.bar.bg = nord[0]

# Background color for downloads with errors.
# Type: QtColor
c.colors.downloads.error.bg = nord[11]

# Foreground color for downloads with errors.
# Type: QtColor
c.colors.downloads.error.fg = nord[5]

# Color gradient start for download backgrounds.
# Type: QtColor
c.colors.downloads.start.bg = nord[10]

# Color gradient start for download text.
# Type: QtColor
c.colors.downloads.start.fg = nord[5]

# Color gradient stop for download backgrounds.
# Type: QtColor
c.colors.downloads.stop.bg = nord[15]

# Color gradient end for download text.
# Type: QtColor
c.colors.downloads.stop.fg = nord[5]

# Color gradient interpolation system for download backgrounds.
# Type: ColorSystem
# Valid values:
#   - rgb: Interpolate in the RGB color system.
#   - hsv: Interpolate in the HSV color system.
#   - hsl: Interpolate in the HSL color system.
#   - none: Don't show a gradient.
c.colors.downloads.system.bg = 'rgb'

# Color gradient interpolation system for download text.
# Type: ColorSystem
# Valid values:
#   - rgb: Interpolate in the RGB color system.
#   - hsv: Interpolate in the HSV color system.
#   - hsl: Interpolate in the HSL color system.
#   - none: Don't show a gradient.
c.colors.downloads.system.fg = 'rgb'

# Background color for hints. Note that you can use a `rgba(...)` value
# for transparency.
# Type: QssColor
c.colors.hints.bg = nord[13]

# Font color for hints.
# Type: QssColor
c.colors.hints.fg = nord[0]

# Font color for the matched part of hints.
# Type: QssColor
c.colors.hints.match.fg = nord[10]

# Background color of the keyhint widget.
# Type: QssColor
c.colors.keyhint.bg = nord[1]

# Text color for the keyhint widget.
# Type: QssColor
c.colors.keyhint.fg = nord[5]

# Highlight color for keys to complete the current keychain.
# Type: QssColor
c.colors.keyhint.suffix.fg = nord[13]

# Background color of an error message.
# Type: QssColor
c.colors.messages.error.bg = nord[11]

# Border color of an error message.
# Type: QssColor
c.colors.messages.error.border = nord[11]

# Foreground color of an error message.
# Type: QssColor
c.colors.messages.error.fg = nord[5]

# Background color of an info message.
# Type: QssColor
c.colors.messages.info.bg = nord[8]

# Border color of an info message.
# Type: QssColor
c.colors.messages.info.border = nord[8]

# Foreground color an info message.
# Type: QssColor
c.colors.messages.info.fg = nord[5]

# Background color of a warning message.
# Type: QssColor
c.colors.messages.warning.bg = nord[12]

# Border color of a warning message.
# Type: QssColor
c.colors.messages.warning.border = nord[12]

# Foreground color a warning message.
# Type: QssColor
c.colors.messages.warning.fg = nord[5]

# Background color for prompts.
# Type: QssColor
c.colors.prompts.bg = nord[2]

# ## Border used around UI elements in prompts.
# ## Type: String
c.colors.prompts.border = '1px solid ' + nord[0]

# Foreground color for prompts.
# Type: QssColor
c.colors.prompts.fg = nord[5]

# Background color for the selected item in filename prompts.
# Type: QssColor
c.colors.prompts.selected.bg = nord[3]

# Background color of the statusbar in caret mode.
# Type: QssColor
c.colors.statusbar.caret.bg = nord[15]

# Foreground color of the statusbar in caret mode.
# Type: QssColor
c.colors.statusbar.caret.fg = nord[5]

# Background color of the statusbar in caret mode with a selection.
# Type: QssColor
c.colors.statusbar.caret.selection.bg = nord[15]

# Foreground color of the statusbar in caret mode with a selection.
# Type: QssColor
c.colors.statusbar.caret.selection.fg = nord[5]

# Background color of the statusbar in command mode.
# Type: QssColor
c.colors.statusbar.command.bg = nord[2]

# Foreground color of the statusbar in command mode.
# Type: QssColor
c.colors.statusbar.command.fg = nord[5]

# Background color of the statusbar in private browsing + command mode.
# Type: QssColor
c.colors.statusbar.command.private.bg = nord[2]

# Foreground color of the statusbar in private browsing + command mode.
# Type: QssColor
c.colors.statusbar.command.private.fg = nord[5]

# Background color of the statusbar in insert mode.
# Type: QssColor
c.colors.statusbar.insert.bg = nord[14]

# Foreground color of the statusbar in insert mode.
# Type: QssColor
c.colors.statusbar.insert.fg = nord[1]

# Background color of the statusbar.
# Type: QssColor
c.colors.statusbar.normal.bg = nord[0]

# Foreground color of the statusbar.
# Type: QssColor
c.colors.statusbar.normal.fg = nord[5]

# Background color of the statusbar in passthrough mode.
# Type: QssColor
c.colors.statusbar.passthrough.bg = nord[10]

# Foreground color of the statusbar in passthrough mode.
# Type: QssColor
c.colors.statusbar.passthrough.fg = nord[5]

# Background color of the statusbar in private browsing mode.
# Type: QssColor
c.colors.statusbar.private.bg = nord[3]

# Foreground color of the statusbar in private browsing mode.
# Type: QssColor
c.colors.statusbar.private.fg = nord[5]

# Background color of the progress bar.
# Type: QssColor
c.colors.statusbar.progress.bg = nord[5]

# Foreground color of the URL in the statusbar on error.
# Type: QssColor
c.colors.statusbar.url.error.fg = nord[11]

# Default foreground color of the URL in the statusbar.
# Type: QssColor
c.colors.statusbar.url.fg = nord[5]

# Foreground color of the URL in the statusbar for hovered links.
# Type: QssColor
c.colors.statusbar.url.hover.fg = nord[8]

# Foreground color of the URL in the statusbar on successful load
# (http).
# Type: QssColor
c.colors.statusbar.url.success.http.fg = nord[5]

# Foreground color of the URL in the statusbar on successful load
# (https).
# Type: QssColor
c.colors.statusbar.url.success.https.fg = nord[14]

# Foreground color of the URL in the statusbar when there's a warning.
# Type: QssColor
c.colors.statusbar.url.warn.fg = nord[12]

# Background color of the tab bar.
# Type: QtColor
c.colors.tabs.bar.bg = nord[3]

# Background color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.bg = nord[3]

# Foreground color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.fg = nord[5]

# Color for the tab indicator on errors.
# Type: QtColor
c.colors.tabs.indicator.error = nord[11]

# Color gradient start for the tab indicator.
# Type: QtColor
# c.colors.tabs.indicator.start = nord['violet']

# Color gradient end for the tab indicator.
# Type: QtColor
# c.colors.tabs.indicator.stop = nord['orange']

# Color gradient interpolation system for the tab indicator.
# Type: ColorSystem
# Valid values:
#   - rgb: Interpolate in the RGB color system.
#   - hsv: Interpolate in the HSV color system.
#   - hsl: Interpolate in the HSL color system.
#   - none: Don't show a gradient.
c.colors.tabs.indicator.system = 'rgb'

# Background color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.bg = nord[3]

# Foreground color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.fg = nord[5]

# Background color of pinned unselected even tabs.
# Type: QtColor
c.colors.tabs.pinned.even.bg = nord[7]

# Foreground color of pinned unselected even tabs.
# Type: QtColor
c.colors.tabs.pinned.even.fg = nord[4]

# Background color of pinned unselected odd tabs.
# Type: QtColor
c.colors.tabs.pinned.odd.bg = nord[8]

# Foreground color of pinned unselected odd tabs.
# Type: QtColor
c.colors.tabs.pinned.odd.fg = nord[4]

# Background color of pinned selected even tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.even.bg = nord[1]

# Foreground color of pinned selected even tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.even.fg = nord[4]

# Background color of pinned selected odd tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.odd.bg = nord[1]

# Foreground color of pinned selected odd tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.odd.fg = nord[5]

# ## Background color of selected even tabs.
# ## Type: QtColor
c.colors.tabs.selected.even.bg = nord[0]

# ## Foreground color of selected even tabs.
# ## Type: QtColor
c.colors.tabs.selected.even.fg = nord[5]

# ## Background color of selected odd tabs.
# ## Type: QtColor
c.colors.tabs.selected.odd.bg = nord[0]

# ## Foreground color of selected odd tabs.
# ## Type: QtColor
c.colors.tabs.selected.odd.fg = nord[5]

# Background color for webpages if unset (or empty to use the theme's
# color).
# Type: QtColor
c.colors.webpage.bg = nord[1]

# Which algorithm to use for modifying how colors are rendered with
# darkmode. The `lightness-cielab` value was added with QtWebEngine 5.14
# and is treated like `lightness-hsl` with older QtWebEngine versions.
# Type: String
# Valid values:
#   - lightness-cielab: Modify colors by converting them to CIELAB color space and inverting the L value. Not available with Qt < 5.14.
#   - lightness-hsl: Modify colors by converting them to the HSL color space and inverting the lightness (i.e. the "L" in HSL).
#   - brightness-rgb: Modify colors by subtracting each of r, g, and b from their maximum value.
c.colors.webpage.darkmode.algorithm = 'lightness-cielab'

# Contrast for dark mode. This only has an effect when
# `colors.webpage.darkmode.algorithm` is set to `lightness-hsl` or
# `brightness-rgb`.
# Type: Float
c.colors.webpage.darkmode.contrast = 0.0

# Render all web contents using a dark theme. Example configurations
# from Chromium's `chrome://flags`:  - "With simple HSL/CIELAB/RGB-based
# inversion": Set   `colors.webpage.darkmode.algorithm` accordingly.  -
# "With selective image inversion": Set
# `colors.webpage.darkmode.policy.images` to `smart`.  - "With selective
# inversion of non-image elements": Set
# `colors.webpage.darkmode.threshold.text` to 150 and
# `colors.webpage.darkmode.threshold.background` to 205.  - "With
# selective inversion of everything": Combines the two variants   above.
# Type: Bool
c.colors.webpage.darkmode.enabled = False

# Which images to apply dark mode to. With QtWebEngine 5.15.0, this
# setting can cause frequent renderer process crashes due to a
# https://codereview.qt-project.org/c/qt/qtwebengine-
# chromium/+/304211[bug in Qt].
# Type: String
# Valid values:
#   - always: Apply dark mode filter to all images.
#   - never: Never apply dark mode filter to any images.
#   - smart: Apply dark mode based on image content. Not available with Qt 5.15.0.
c.colors.webpage.darkmode.policy.images = 'smart'

# Which pages to apply dark mode to. The underlying Chromium setting has
# been removed in QtWebEngine 5.15.3, thus this setting is ignored
# there. Instead, every element is now classified individually.
# Type: String
# Valid values:
#   - always: Apply dark mode filter to all frames, regardless of content.
#   - smart: Apply dark mode filter to frames based on background color.
c.colors.webpage.darkmode.policy.page = 'smart'

# Threshold for inverting background elements with dark mode. Background
# elements with brightness above this threshold will be inverted, and
# below it will be left as in the original, non-dark-mode page. Set to
# 256 to never invert the color or to 0 to always invert it. Note: This
# behavior is the opposite of `colors.webpage.darkmode.threshold.text`!
# Type: Int
c.colors.webpage.darkmode.threshold.background = 0

# Value to use for `prefers-color-scheme:` for websites. The "light"
# value is only available with QtWebEngine 5.15.2+. On older versions,
# it is the same as "auto". The "auto" value is broken on QtWebEngine
# 5.15.2 due to a Qt bug. There, it will fall back to "light"
# unconditionally.
# Type: String
# Valid values:
#   - auto: Use the system-wide color scheme setting.
#   - light: Force a light theme.
#   - dark: Force a dark theme.
c.colors.webpage.preferred_color_scheme = 'auto'

