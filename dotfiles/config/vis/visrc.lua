require('vis')
local uni = require('uni')

vis.events.subscribe(vis.events.INIT, function()
	-- Your global configuration options
	vis:command('set theme light-16')
	vis:command('set autoindent')
        uni:mapKeys()
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	-- Your per window configuration options e.g.
	-- vis:command('set number')
end)
