" $BJ8;z%(%s%3!<%G%#%s%0$N@_Dj(B
set encoding=utf-8
"set encoding=cp932

set fileencodings=utf-8,iso-2022-jp,iso-2022-jp-2,euc-jp,sjis

" $B%U%!%$%kKh$K0[$J$k9=J8$d%W%i%0%$%s$r;H$&$h$&$K$9$k(B
syntax on
filetype plugin indent on

" $B%+%i!<%9%-!<%^$N@_Dj(B
colorscheme zenburn

highlight LineNr ctermfg=grey
highlight NonText ctermfg=darkgrey
highlight Folded ctermfg=blue
highlight SpecialKey cterm=underline ctermfg=darkgrey
highlight Statement ctermfg=white
highlight Comment   ctermfg=yellow
highlight String ctermfg=green cterm=bold

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" $B4pK\E*$J@_Dj(B
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" $B%?%VI}(B
set ts=4 sw=4
set softtabstop=4
set expandtab

" $B?7$7$$9T$N%$%s%G%s%H$r8=:_9T$HF1$8$K$9$k(B
set autoindent

" $B%$%s%/%j%a%s%?%k%5!<%A$r9T$&(B
set incsearch

" $B9THV9f$rI=<($9$k(B
set number

" $B%3%^%s%II=<((B
set showcmd

" $B%9%F!<%?%99TDI2C(B
set laststatus=2
" $B%9%F!<%?%99T$KJ8;z%3!<%I$H2~9T%3!<%II=<((B
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P

" $BF1$83g8L$,F~NO$5$l$?$H$-!"BP1~$9$k3g8L$rI=<($9$k(B
set showmatch

" $B8!:w;~$KBgJ8;z$r4^$s$G$$$?$iBg(B/$B>.$r6hJL(B
set smartcase

" $B?7$7$$9T$r:n$C$?$H$-$K9bEY$J<+F0%$%s%G%s%H$r9T$&(B
set smartindent

" $B9TF,$NM>GrFb$G(BTab$B$rBG$A9~$`$H!"(B'shiftwidth'$B$N?t$@$1%$%s%G%s%H$9$k(B
set smarttab

" $B%+!<%=%k$r9TF,!"9TKv$G;_$^$i$J$$$h$&$K$9$k(B
set whichwrap=b,s,h,l,<,>,[,]

" $B8!:w$r%U%!%$%k$N@hF,$X%k!<%W$7$J$$(B
set nowrapscan

" $BJT=8Cf$NFbMF$rJ]$C$?$^$^JL$N2hLL$K@Z$jBX$($i$l$k$h$&$K$9$k(B
set hid

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" $B%-!<%P%$%s%I@_Dj(B
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" $BI=<(9TC10L$G0\F0(B
nnoremap j gj
nnoremap k gk

" <F2>: $BA0$N%P%C%U%!(B
" <F3>: $B<!$N%P%C%U%!(B
" <F4>: $B%P%C%U%!$r:o=|(B
map <F2> <ESC>:bp<CR>
map <F3> <ESC>:bn<CR>
map <F4> <ESC>:bw<CR>

" $B8!:w8l$r2hLLCf1{$K(B
nmap n nzz
nmap N Nzz
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" $B%*!<%H%3%^%s%I@_Dj(B
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,exept,finally,def,class

" $BF~NO%b!<%I;~!"%9%F!<%?%9%i%$%s$N%+%i!<$rJQ99(B
"augroup InsertHook
"autocmd!
"autocmd InsertEnter * highlight StatusLine guifg=#ccdc90
"autocmd InsertLeave * highlight StatusLine guifg=#2E434C

" $BF|K\8lF~NO$r%j%;%C%H(B
au BufNewFile,BufRead * set iminsert=0
" $B%?%VI}$r%j%;%C%H(B
au BufNewFile,BufRead * set tabstop=4 shiftwidth=4

" .rhtml$B$H(B.rb$B$G%?%VI}$rJQ99(B
au BufNewFile,BufRead *.rhtml set nowrap tabstop=2 shiftwidth=2
au BufNewFile,BufRead *.rb set nowrap tabstop=2 shiftwidth=2

" $BA02s=*N;$7$?%+!<%=%k9T$K0\F0(B
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

"----------------------------------------
" Vim$B%9%/%j%W%H(B
"----------------------------------------
" $BJ8;z%3!<%I$N<+F0G'<1(B
" http://www.kawaz.jp/pukiwiki/?vim#content_1_7
if &encoding !=# 'utf-8'
  set encoding=japan
  set fileencoding=japan
endif
if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'
  " iconv$B$,(BeucJP-ms$B$KBP1~$7$F$$$k$+$r%A%'%C%/(B
  if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'eucjp-ms'
    let s:enc_jis = 'iso-2022-jp-3'
  " iconv$B$,(BJISX0213$B$KBP1~$7$F$$$k$+$r%A%'%C%/(B
  elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213'
    let s:enc_jis = 'iso-2022-jp-3'
  endif
  " fileencodings$B$r9=C[(B
  if &encoding ==# 'utf-8'
    let s:fileencodings_default = &fileencodings
    let &fileencodings = s:enc_jis .','. s:enc_euc .',cp932'
    let &fileencodings = &fileencodings .','. s:fileencodings_default
    unlet s:fileencodings_default
  else
    let &fileencodings = &fileencodings .','. s:enc_jis
    set fileencodings+=utf-8,ucs-2le,ucs-2
    if &encoding =~# '^\(euc-jp\|euc-jisx0213\|eucjp-ms\)$'
      set fileencodings+=cp932
      set fileencodings-=euc-jp
      set fileencodings-=euc-jisx0213
      set fileencodings-=eucjp-ms
      let &encoding = s:enc_euc
      let &fileencoding = s:enc_euc
    else
      let &fileencodings = &fileencodings .','. s:enc_euc
    endif
  endif
  " $BDj?t$r=hJ,(B
  unlet s:enc_euc
  unlet s:enc_jis
endif
" $BF|K\8l$r4^$^$J$$>l9g$O(B fileencoding $B$K(B encoding $B$r;H$&$h$&$K$9$k(B
if has('autocmd')
  function! AU_ReCheck_FENC()
    if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
      let &fileencoding=&encoding
    endif
  endfunction
  autocmd BufReadPost * call AU_ReCheck_FENC()
endif
" $B2~9T%3!<%I$N<+F0G'<1(B
set fileformats=unix,dos,mac
" $B""$H$+!{$NJ8;z$,$"$C$F$b%+!<%=%k0LCV$,$:$l$J$$$h$&$K$9$k(B
if exists('&ambiwidth')
  set ambiwidth=double
endif

""""""""""""""""""""""""""""""
"$BA^F~%b!<%I;~!"%9%F!<%?%9%i%$%s$N?'$rJQ99(B
""""""""""""""""""""""""""""""
let g:hi_insert = 'highlight StatusLine guifg=darkblue guibg=darkyellow gui=none ctermfg=blue ctermbg=yellow cterm=none'

if has('syntax')
  augroup InsertHook
    autocmd!
    autocmd InsertEnter * call s:StatusLine('Enter')
    autocmd InsertLeave * call s:StatusLine('Leave')
  augroup END
endif
let s:slhlcmd = ''

function! s:StatusLine(mode)
  if a:mode == 'Enter'
    silent! let s:slhlcmd = 'highlight ' . s:GetHighlight('StatusLine')
    silent exec g:hi_insert
  else
    highlight clear StatusLine
    silent exec s:slhlcmd
  endif
endfunction

function! s:GetHighlight(hi)
  redir => hl
  exec 'highlight '.a:hi
  redir END
  let hl = substitute(hl, '[\r\n]', '', 'g')
  let hl = substitute(hl, 'xxx', '', '')
  return hl
endfunction

