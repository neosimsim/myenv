require('vis')
local uni = require('uni')

vis.events.subscribe(vis.events.INIT, function()
  uni:mapKeys()
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
  vis:command('set autoindent')
end)

local function rmTrailingBlanks(file, path)
  local win = vis.win
  local pos = win.selection.pos
  local status, out, err = vis:pipe(file, { start = 0, finish = file.size }, 'sed \'s/[[:blank:]]*$//\'')
  if status ~= 0 or not out then
   if err then vis:info(err) end
   return false
  end
  file:delete(0, file.size)
  file:insert(0, out)
  win.selection.pos = math.min(pos, file.size - 1)
  return true
end

vis.events.subscribe(vis.events.FILE_SAVE_PRE, rmTrailingBlanks)