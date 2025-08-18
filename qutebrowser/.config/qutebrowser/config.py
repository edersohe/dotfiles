config.load_autoconfig(True)
config.set("window.hide_decoration", True)
config.set('content.cookies.accept', 'all', 'chrome-devtools://*')
config.set('content.cookies.accept', 'all', 'devtools://*')
config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')
config.set('colors.webpage.darkmode.enabled', True)
c.tabs.position = 'top'
c.url.default_page = 'https://duckduckgo.com/'
c.url.searchengines = {'DEFAULT': 'https://duckduckgo.com/?q={}', 'g': 'https://google.com/search?q={}'}
c.url.start_pages = 'https://duckduckgo.com/'

font = 'Hasklug Nerd Font'

c.fonts.default_family = font
c.fonts.default_size = "12pt"

#c.zoom.default = 125

import os
from urllib.request import urlopen

# load your autoconfig, use this, if the rest of your config is empty!
config.load_autoconfig()

import catppuccin
catppuccin.setup(c, 'mocha', True)

