""" vundle.vim
set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" インストールするプラグイン
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/unite.vim'
Bundle 'tsukkee/unite-help'
Bundle 'surround.vim'
Bundle 'rails.vim'
Bundle 'ruby-matchit'
Bundle 'matchit.zip'
Bundle 'quickrun.vim'
Bundle 'ref.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'ZenCoding.vim'

Bundle 'vim-ruby/vim-ruby'
Bundle 'motemen/git-vim'

Bundle 'Zenburn'

filetype plugin indent on

" ファイル毎に異なる構文やプラグインを使うようにする
syntax on

" 文字エンコーディングの設定
set encoding=utf-8
"set encoding=cp932

set fileencodings=utf-8,iso-2022-jp,iso-2022-jp-2,euc-jp,sjis

" カラースキーマの設定
colorscheme zenburn

highlight LineNr        ctermfg=grey
highlight NonText       ctermfg=darkgrey
highlight Folded        ctermfg=blue
highlight SpecialKey    cterm=underline ctermfg=darkgrey
highlight Statement     ctermfg=white
highlight Comment       ctermfg=yellow
highlight String        ctermfg=green   cterm=bold

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 基本的な設定
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" タブ幅
set shiftwidth=4
set softtabstop=4
set expandtab

" 新しい行のインデントを現在行と同じにする
set autoindent

" インクリメンタルサーチを行う
set incsearch

" 検索語を強調表示（<C-L>を押すと現在の強調表示を解除する）
set hlsearch

" 行番号を表示する
set number

" コマンド表示
set showcmd

" ステータス行追加
set laststatus=2
" ステータス行に文字コードと改行コード表示
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P

" 同じ括弧が入力されたとき、対応する括弧を表示する
set showmatch

" 検索時に大文字を含んでいたら大/小を区別
set smartcase

" 新しい行を作ったときに高度な自動インデントを行う
set smartindent

" 行頭の余白内でTabを打ち込むと、'shiftwidth'の数だけインデントする
set smarttab

" カーソルを行頭、行末で止まらないようにする
set whichwrap=b,s,h,l,<,>,[,]

" 検索をファイルの先頭へループしない
set nowrapscan

" 編集中の内容を保ったまま別の画面に切り替えられるようにする
set hidden

" コマンドライン補完を便利に
set wildmenu

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" キーバインド設定
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 表示行単位で移動
nnoremap j gj
nnoremap k gk

" スペースキーでページ送り
nnoremap <SPACE> <C-f>
nnoremap <S-SPACE> <C-b>

" <F2>: 前のバッファ
" <F3>: 次のバッファ
" <F4>: バッファを削除
map <F2> <ESC>:bp<CR>
map <F3> <ESC>:bn<CR>
map <F4> <ESC>:bw<CR>

" 検索語を画面中央に
nmap n nzz
nmap N Nzz

" <C-L>で検索後の強調表示を解除する
nnoremap <C-L> :nohl<CR><C-L>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" プラグイン設定
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" ref.vim
nmap ,rr :<C-u>Ref refe<Space>
nmap ,ra :<C-u>Ref alc<Space>

let g:ref_alc_start_linenumber = 39  "表示する行数
let g:ref_alc_encoding = 'UTF-8' "文字コード

""" quickrun.vim
" RVMで入れたRubyを使う
" http://d.hatena.ne.jp/uasi/20110411/1302531017
let g:quickrun_config = {}

if strlen($rvm_bin_path)
	let g:quickrun_config['ruby'] = {
\		'command': 'ruby',
\		'exec': '$rvm_bin_path/ruby %s',
\		'tempfile': '{tempname()}.rb'
\	}
endif

" neocomplcache
let g:neocomplcache_enable_at_startup = 1 " 起動時に有効化

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" オートコマンド設定
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,exept,finally,def,class

" 入力モード時、ステータスラインのカラーを変更
"augroup InsertHook
"autocmd!
"autocmd InsertEnter * highlight StatusLine guifg=#ccdc90
"autocmd InsertLeave * highlight StatusLine guifg=#2E434C

" 日本語入力をリセット
au BufNewFile,BufRead * set iminsert=0
" タブ幅をリセット
au BufNewFile,BufRead * set tabstop=4 shiftwidth=4

" .rhtmlと.rbでタブ幅を変更
au BufNewFile,BufRead *.rhtml set nowrap tabstop=2 shiftwidth=2
au BufNewFile,BufRead *.rb set nowrap tabstop=2 shiftwidth=2

" 前回終了したカーソル行に移動
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vimスクリプト
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 文字コードの自動認識
" http://www.kawaz.jp/pukiwiki/?vim#content_1_7
if &encoding !=# 'utf-8'
  set encoding=japan
  set fileencoding=japan
endif
if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'
  " iconvがeucJP-msに対応しているかをチェック
  if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'eucjp-ms'
    let s:enc_jis = 'iso-2022-jp-3'
  " iconvがJISX0213に対応しているかをチェック
  elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213'
    let s:enc_jis = 'iso-2022-jp-3'
  endif
  " fileencodingsを構築
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
  " 定数を処分
  unlet s:enc_euc
  unlet s:enc_jis
endif
" 日本語を含まない場合は fileencoding に encoding を使うようにする
if has('autocmd')
  function! AU_ReCheck_FENC()
    if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
      let &fileencoding=&encoding
    endif
  endfunction
  autocmd BufReadPost * call AU_ReCheck_FENC()
endif
" 改行コードの自動認識
set fileformats=unix,dos,mac
" □とか○の文字があってもカーソル位置がずれないようにする
if exists('&ambiwidth')
  set ambiwidth=double
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 挿入モード時、ステータスラインの色を変更
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"  Open junk file. via.http://vim-users.jp/2010/11/hack181/
command! -nargs=0 JunkFile call s:open_junk_file()
function! s:open_junk_file()
  let l:junk_dir = $HOME . '/junk'. strftime('/%Y')
  if !isdirectory(l:junk_dir)
    call mkdir(l:junk_dir, 'p')
  endif

  let l:filename = input('Junk Code: ', l:junk_dir.strftime('/%Y-%m-%d-%H%M%S.'))
  if l:filename != ''
    execute 'split ' . l:filename
  endif
endfunction

command! -nargs=0 JunkFileDay call s:open_junk_file_day()
function! s:open_junk_file_day()
  let l:junk_dir = $HOME . '/junk'. strftime('/%Y')
  if !isdirectory(l:junk_dir)
    call mkdir(l:junk_dir, 'p')
  endif

  let l:filename = l:junk_dir.strftime('/%Y-%m-%d.txt')
  if l:filename != ''
    execute 'split ' . l:filename
  endif
endfunction

nnoremap ,jf :JunkFile
nnoremap ,j  :JunkFileDay
