config.source('common.py')

c.url.default_page = 'https://google.com/'
c.url.start_pages = 'https://google.com/'
c.url.searchengines = {'DEFAULT': 'https://google.com/search?q={}', 'b': 'https://search.brave.com/search?q={}', 'd': 'https://duckduckgo.com/?q={}'}
