set expandtab
set ts=2
set sts=2

if executable('haskell-language-server-wrapper')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'hls',
        \ 'cmd': {server_info->['haskell-language-server-wrapper', '--lsp']},
        \ 'allowlist': ['haskell', 'lhaskell'],
        \ })
endif
