config.load_autoconfig(True)
config.set("window.hide_decoration", True)
config.set('colors.webpage.darkmode.enabled', True)

c.tabs.position = 'top'

font = 'Hasklug Nerd Font'
c.fonts.default_family = font
c.fonts.default_size = "11pt"

import catppuccin
catppuccin.setup(c, 'mocha', True)

