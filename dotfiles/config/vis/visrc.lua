require('vis')
local uni = require('uni')

vis.events.subscribe(vis.events.INIT, function()
  uni:mapKeys()
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
  vis:command('set autoindent')
  vis:command('set syntax off')
end)

local function fmt(file, path)
  local win = vis.win

  local fmtCmd = 'sed \'s/[[:blank:]]*$//\''
  if win.syntax == "haskell" then
    fmtCmd = "ormolu"
  elseif win.syntax == "elixir" then
    fmtCmd = "mix format -"
  end

  local pos = win.selection.pos
  local status, out, err = vis:pipe(file, { start = 0, finish = file.size }, fmtCmd)
  if status ~= 0 or not out then
   if err then
     vis:info(err)
   else
     vis:info("error running " .. fmtCmd)
   end
   return true
  end

  file:delete(0, file.size)
  file:insert(0, out)
  win.selection.pos = math.min(pos, file.size - 1)
  return true
end

vis.events.subscribe(vis.events.FILE_SAVE_PRE, fmt)