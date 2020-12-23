require('vis')
local uni = require('uni')

vis.events.subscribe(vis.events.INIT, function()
  uni:mapKeys()
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
  vis:command('set autoindent')
end)
