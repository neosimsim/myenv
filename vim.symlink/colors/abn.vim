set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "abn"

hi Error		ctermbg=1 ctermfg=0
hi Special		cterm=none
hi LineNr		ctermfg=7
hi SpellBad		ctermbg=1 ctermfg=0
hi DiffAdd		ctermfg=0 ctermbg=2
hi DiffDelete	ctermfg=0 ctermbg=1
hi DiffChange	ctermfg=0 ctermbg=3
hi DiffText		ctermfg=3 ctermbg=0
