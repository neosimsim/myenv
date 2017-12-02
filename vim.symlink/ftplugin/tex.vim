" this is mostly a matter of taste. but LaTeX looks good with just a bit
" of indentation.
set sw=4
set tabstop=4
set textwidth=0
" TIP: if you write your \label's as \label{fig:something}, then if you
" type in \ref{fig: and press <C-n> you will automatically cycle through
" all the figure labels. Very useful!
set iskeyword+=:

let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_FormatDependency_pdf = 'pdf'
let g:Tex_CompileRule_pdf = 'pdflatex --shell-escape --interaction=nonstopmode $*'
let g:Tex_MultipleCompileFormats='pdf'
let g:Tex_FoldedEnvironments="verbatim,comment,eq,gather,align,figure,table,thebibliography,keywords,abstract,titlepage,description"

let g:Tex_BibtexFlavor = 'biber'
let g:Tex_CompileRule_bib = 'biber $*'

let g:Tex_ViewRule_pdf = 'mupdf -r189'

let g:Tex_SmartQuoteOpen="\"`"
let g:Tex_SmartQuoteClose="\"'"
let g:Tex_SmartKeyQuote=0

let g:Tex_Env_theorem = "\\begin{theorem}[<++>]\\label{thm:<++>}\<CR><++>\<CR>\\end{theorem}"
let g:Tex_Env_definition = "\\begin{definition}[<++>]\\label{def:<++>}\<CR><++>\<CR>\\end{definition}"
let g:Tex_Env_lemma = "\\begin{lemma}\\label{lem:<++>}\<CR><++>\<CR>\\end{lemma}"
" let g:Tex_Env_lemma! = "\\begin{lemma!}\\label{lem:<++>}\<CR><++>\<CR>\\end{lemma!}"
let g:Tex_Env_remark = "\\begin{remark}\\label{rem:<++>}\<CR><++>\<CR>\\end{remark}"
let g:Tex_Env_corollary = "\\begin{corrolary}\\label{cor:<++>}\<CR><++>\<CR>\\end{corrolary}"
" let g:Tex_Env_corollary! = "\\begin{corrolary!}[<++>]\\label{cor:<++>}\<CR><++>\<CR>\\end{corrolary!}"
let g:Tex_Env_example = "\\begin{example}[<++>]\\label{ex:<++>}\<CR><++>\<CR>\\end{example}"

let g:Tex_Com_inner = "\\inner[<++>]{<++>}{<++>}<++>"

set spell
set conceallevel=0
call IMAP('NOM', '\nomenclature{<++>}<++>', 'tex')
call IMAP('`1', '\reci{<++>}<++>', 'tex')
