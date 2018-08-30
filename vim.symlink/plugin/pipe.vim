" pipe.vim  -  Pipe range to unix command and replace with output
"
" Copyright © 2018, Alexander Ben Nasrallah <me@abn.sh>
" Use of this source code is governed by a BSD 3-clause
" style license that can be found in the LICENSE file.
"
" Usage:
"
" :[unix]Pipe {unix command}

if exists('g:loaded_pipe')
  finish
endif
let g:loaded_pipe = 1

function! Pipe(...) range
	let l:cmd = join(a:000)
	let l:input = getline(a:firstline, a:lastline)
	call setpos('.', [0, a:lastline, 1, 0])
	silent put =system(l:cmd, l:input)
	silent execute a:firstline . "," . a:lastline . "d _"
endfunction

function! PipeOut(...) range
	let l:cmd = join(a:000)
	let l:input = getline(a:firstline, a:lastline)
	call setpos('.', [0, a:lastline, 1, 0])
	let out = system(l:cmd, l:input)
	echo out
endfunction

function! PipeIn(...) range
	let l:cmd = join(a:000)
	call setpos('.', [0, a:lastline, 1, 0])
	silent put =system(l:cmd)
	silent execute a:firstline . "," . a:lastline . "d _"
endfunction

command! -range -nargs=+ -complete=shellcmd Pipe <line1>,<line2>call Pipe(<q-args>)
command! -range -nargs=+ -complete=shellcmd PipeOut <line1>,<line2>call PipeOut(<q-args>)
command! -range -nargs=+ -complete=shellcmd PipeIn <line1>,<line2>call PipeIn(<q-args>)

