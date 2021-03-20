require('vis')
local uni = require('uni')

vis.events.subscribe(vis.events.INIT, function()
  uni:mapKeys()
  vis:command('set theme peaksea')
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
  vis:command('set autoindent')
  vis:command('set syntax off')
end)

local function fmt(file, path)
  local fmtCmd = 'sed \'s/[[:blank:]]*$//\''
  if path:match("%.hs$") then
    fmtCmd = "ormolu"
  elseif path:match("%.exs?$") then
    fmtCmd = "mix format -"
  elseif path:match("%.cabal$") then
    fmtCmd = "cabal-fmt"
  end

  local win = vis.win
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