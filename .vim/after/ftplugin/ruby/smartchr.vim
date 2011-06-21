" ()や""などは複数回打鍵で内側にカーソルを移動
inoremap <buffer> <expr> " smartchr#one_of('"', '""<Left>')
inoremap <buffer> <expr> ' smartchr#one_of("'", "''<Left>")
inoremap <buffer> <expr> <bar> smartchr#loop('<bar>', '<bar><bar>', '<bar><bar><Left>')
inoremap <buffer> <expr> ( smartchr#one_of('(', '()<Left>')
inoremap <buffer> <expr> [ smartchr#one_of('[', '[]<Left>')
inoremap <buffer> <expr> / search(':/\?\%#', 'bcn') ? '/' : smartchr#one_of('/', '//<Left>')

inoremap <buffer> <expr> > smartchr#loop('>', '=>', '<=>', '>>')
" #は文字列内なら#{}
inoremap <buffer> <expr> # search('".*\%#', 'bcn') && search('\%#.*"', 'cn') ? '#{}<Left>' : smartchr#one_of('# ', '## ')
" += などの場合は = の前に空白を入れない
inoremap <buffer><expr> = search('\(&\<bar><bar>\<bar>+\<bar>-\<bar>*\<bar>/\<bar>>\<bar><\<bar>!\)\%#', 'bcn')? '= '
                \ : search('".*\%#', 'bcn') && search('\%#.*"', 'cn') ? '='
                \ : smartchr#one_of(' = ', ' == ', ' =~ ', '=== ')

" {を2連打で{|var|、}を2連打で改行
inoremap <buffer><expr> { smartchr#one_of('{ ', '{ <bar><bar><Left>')
inoremap <buffer><expr> } smartchr#one_of(' }', ' }<CR>')
