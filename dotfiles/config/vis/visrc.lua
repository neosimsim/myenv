require('vis')
local uni = require('uni')

vis.events.subscribe(vis.events.INIT, function()
  vis:command('set autoindent')
  uni:mapKeys()
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	-- Your per window configuration options e.g.
	-- vis:command('set number')
end)
