require('vis')
local uni = require('plugins/uni')

local plug = require('plugins/vis-plug')
plug.init({
  { url = 'git.sr.ht/~mcepl/vis-fzf-open' },
})

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
  elseif path:match("%.rs$") then
    fmtCmd = "rustfmt"
  elseif path:match("%.go$") then
    fmtCmd = "gofmt"
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

vis:map(vis.modes.INSERT, "<C-f>", function()
	local win = vis.win
	local file = win.file
	local pos = win.selection.pos
	if not pos then return end
	local range = file:text_object_word(pos > 0 and pos-1 or pos);
	if not range then return end
	if range.finish > pos then range.finish = pos end
	if range.start == range.finish then return end
	local prefix = file:content(range)
	if not prefix then return end
	local cmd = string.format("sed 's/[[:blank:][:punct:]̈]/\\n/g' | grep -v '^%s$' | sort -u | fzf-tmux --print0 -1 -q '%s'", prefix:gsub("'", "'\''"), prefix:gsub("'", "'\''"))
	local status, out, err = vis:pipe(file, { start = 0, finish = file.size }, cmd)
	if status ~= 0 or not out then
		if err then vis:info(err) end
		return
	end
	-- fzf returns the whole word, so we have to delete the prefix
	file:delete({ start = pos - #prefix, finish = pos })
	file:insert(pos - #prefix, out)
	win.selection.pos = pos - #prefix + #out
end, "Complete word in file with fzf")
