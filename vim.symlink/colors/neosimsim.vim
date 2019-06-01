set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "neosimsim"

hi Error		ctermfg=0 ctermbg=1
hi Special		cterm=none
hi LineNr		ctermfg=7
hi SpellBad		ctermfg=0 ctermbg=1
hi Visual		ctermfg=7 ctermbg=0
hi DiffAdd		ctermfg=0 ctermbg=2
hi DiffDelete	ctermfg=0 ctermbg=1
hi clear DiffChange
hi clear DiffText
hi DiffText		cterm=NONE ctermfg=3
