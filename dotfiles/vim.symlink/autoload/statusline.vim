function statusline#TrailingWhiteSpacesWarning()
	let s:whiteLine = search('\s\+$', 'nw')
	if s:whiteLine == 0
		return ''
	endif
	return ' Trailing: '.s:whiteLine
endfunction

function statusline#MixedIndentWarning()
	let tabs_used = search('^\t', 'nw') != 0
	let spaces_used = search('^ ', 'nw') != 0

	if tabs_used && spaces_used
		return ' Mixed indenting'
	endif
	return ''
endfunction
