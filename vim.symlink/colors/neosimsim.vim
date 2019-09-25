set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "neosimsim"

hi Error      cterm=none ctermfg=0 ctermbg=1
hi Special    cterm=none
hi LineNr     cterm=none ctermfg=7
hi SpellBad   cterm=none ctermfg=0 ctermbg=1
hi Visual     cterm=none ctermfg=7 ctermbg=0
hi DiffAdd    cterm=none ctermfg=0 ctermbg=2
hi DiffDelete cterm=none ctermfg=0 ctermbg=1
hi DiffChange cterm=none ctermfg=0 ctermbg=3
hi DiffText   cterm=none ctermfg=0 ctermbg=4
