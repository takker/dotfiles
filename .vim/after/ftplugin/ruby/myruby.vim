" タブの設定
setlocal expandtab
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=4

setlocal foldmethod=indent

compiler ruby

" ,ccで構文チェック
nmap <buffer> ,cc :<C-u>make -c %<CR>

