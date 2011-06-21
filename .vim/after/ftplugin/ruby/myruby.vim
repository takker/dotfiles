" タブの設定
setlocal expandtab
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=4

setlocal foldmethod=indent

compiler ruby

" ,cで構文チェック
nmap <buffer> ,c :<C-u>make -c %<CR>

