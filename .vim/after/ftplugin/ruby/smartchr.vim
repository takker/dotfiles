inoremap <buffer> <expr> = smartchr#loop(' = ', ' == ', ' === ', '=')
inoremap <buffer> <expr> # smartchr#loop('# ', '#{}<C-o>i')
inoremap <buffer> <expr> " smartchr#loop('""<C-o>i')
inoremap <buffer> <expr> > smartchr#loop(' > ', ' => ', ' <=> ', ' >> ')
inoremap <buffer> <expr> / smartchr#loop(' / ', '//<C-o>i')
inoremap <buffer> <expr> <bar> smartchr#loop('<bar>', '<bar><bar>', '<bar><bar><C-o>i')