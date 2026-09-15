config.load_autoconfig(True)
config.set("window.hide_decoration", True)
config.set('colors.webpage.darkmode.enabled', True)

c.tabs.position = 'top'

font = 'ZedMono Nerd Font'
c.fonts.default_family = font
c.fonts.default_size = "15pt"

config.source('noctalia/colors.py')

# Override: keep the page's original background instead of forcing a dark
# theme. darkmode.enabled = True inverts some pages to dark backgrounds where
# text becomes unreadable. Appended (not replaced) so the previous setting
# stays visible above as context.
config.set('colors.webpage.darkmode.enabled', False)

