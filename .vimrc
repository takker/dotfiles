" <Leader>をスペースに設定
let mapleader = " "

" 特定のファイルを編集するためのキーバインド
nnoremap [edit] <Nop>
nmap ,e [edit]
" .vimrc, .gvimrcをそれぞれ ,ee ,eg で開く
nnoremap <silent> [edit]e :<C-u>e ~/Projects/dotfiles/.vimrc<CR>
nnoremap <silent> [edit]g :<C-u>e ~/Projects/dotfiles/.gvimrc<CR>
" 特定の項目をチェックするためのキーバインド
nnoremap [check] <Nop>
nmap ,c [check]

""" vundle.vim
set nocompatible
filetype off

set rtp& rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" http://d.hatena.ne.jp/ZoAmichi/20110515/1305391268
" 1.bundles.vimでプラグインのリストを管理
" 2.EditBundlesでこのリストの編集を開始
" 3.リストを保存することでg:bundlesを更新してBundleCleanする
let $bundles_file=$HOME.'/.vim/bundles.vim'
com! EditBundles :e $bundles_file
nnoremap <silent> [edit]b :<C-u>EditBundles<CR>

augroup Vundle
  autocmd!
  au BufWritePost $bundles_file call vundle#config#init()
  au BufWritePost $bundles_file source $bundles_file
  au BufWritePost $bundles_file BundleClean
  au BufWritePost $bundles_file BundleInstall
augroup END

source $bundles_file

filetype plugin indent on

" ファイル毎に異なる構文やプラグインを使うようにする
syntax on

" 文字エンコーディングの設定
set encoding=utf-8
"set encoding=cp932

set fileencodings=utf-8,iso-2022-jp,iso-2022-jp-2,euc-jp,sjis

" カラーテーマの設定
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
set tabstop=2
set shiftwidth=2
set softtabstop=4
set expandtab

" 新しい行のインデントを現在行と同じにする
set autoindent

" インクリメンタルサーチを行う
set incsearch

" 検索語を強調表示（<C-L>を押すと現在の強調表示を解除する）
set hlsearch

" 検索時に大文字小文字を無視
set ignorecase

" 行番号を表示する
set number

" フォールドカラムの設定
set foldcolumn=2

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

" 特殊文字を表示する
set list
set listchars=tab:\ \ ,extends:<,trail:\ 

" 検索をファイルの先頭へループしない
set nowrapscan

" 編集中の内容を保ったまま別の画面に切り替えられるようにする
set hidden

" コマンドライン補完を便利に
set wildmenu

" 補完を最長一致、各候補の順に行う
set wildmode=longest,full

" バックアップを作成しない
set nobackup

" ヤンクしたデータをクリップボードにも送る
set clipboard& clipboard+=unnamed

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" キーバインド設定
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" ノーマルモード、インサートモード共通
" emacs風のウィンドウ操作
imap <silent> <C-x>1 <ESC>:only<CR>i
nmap <silent> <C-x>1 :<C-u>only<CR>
imap <silent> <C-x>2 <ESC>:split<CR>i
nmap <silent> <C-x>2 :<C-u>split<CR>
imap <silent> <C-x>3 <ESC>:vsplit<CR>i
nmap <silent> <C-x>3 :<C-u>vsplit<CR>
imap <silent> <C-x>k <ESC>:close<CR>i
nmap <silent> <C-x>k :<C-u>close<CR>
imap <silent> <C-x>o <ESC><C-w>wi
nmap <silent> <C-x>o <C-w>w
imap <silent> <C-x>p <ESC><C-w>pi
nmap <silent> <C-x>p <C-w>p

" emacs風のファイル操作
imap <C-x><C-z> <ESC>:qa!
nmap <C-x><C-z> :<C-u>qa!
imap <C-x><C-s> <ESC>:w!<CR>
nmap <C-x><C-s> :<C-u>w!<CR>
imap <C-x>s     <ESC>:wall<CR>
nmap <C-x>s     :<C-u>wall<CR>
imap <C-x>f     <ESC>:e<Space>
nmap <C-x>f     :<C-u>e<Space>
imap <C-x>i     <ESC>:r<Space>
nmap <C-x>i     :<C-u>r<Space>

" <F2>: 前のバッファ
" <F3>: 次のバッファ
" <F4>: バッファを削除
noremap <F2> <ESC>:bp<CR>
noremap <F3> <ESC>:bn<CR>
noremap <F4> <ESC>:bw<CR>

""" ノーマルモード
" +, -でカーソル位置を画面中央に保ったまま移動
nnoremap + jzz
nnoremap - kzz

" 表示行単位で移動
nnoremap j gj
nnoremap k gk

" スペースキーでページ送り
" nnoremap <Space> 8jzz
" nnoremap <S-SPACE> 8kzz

" ページ送りしたときにカーソルを画面の中央位置に
nnoremap <C-f> <C-f>zz
nnoremap <C-b> <C-b>zz

" 検索語を画面中央に
nnoremap n nzz
nnoremap N Nzz

" <C-L>で検索後の強調表示を解除する
nnoremap <C-L> :noh<CR><C-L>

" ノーマルモードでも<CR>で改行を挿入
noremap <CR> o<Esc>

" <Leader><Leader>で末尾の空白を削除し、変更があれば保存
noremap <silent> <Leader><Leader> :<C-u>%s/\s\+$//eg<CR>:noh<CR>:up<CR>

" カーソル下のsyntax名を表示
nnoremap [check]s :<C-u>echo synIDattr(synID(line('.'), col('.'), 0), 'name')<CR>

" mark, register, buffersの内容を表示
nnoremap [check]m :<C-u>marks<CR>
nnoremap [check]r :<C-u>registers<CR>
nnoremap [check]b :<C-u>ls<CR>:buffer<Space>

" gcで最後に変更されたテキストを選択する
nnoremap gc `[v']

" ,reでvimスクリプトを再読込
nnoremap <silent> ,re :<C-u>execute "source " expand("%:p")<CR>

" http://whileimautomaton.net/2008/08/vimworkshop3-kana-presentation
" <C-h>でヘルプ
nnoremap <C-h> :<C-u>help<Space>

" tap pagesの操作
nnoremap <C-t> <Nop>
nnoremap <C-t>n :<C-u>tabnew<CR>
nnoremap <C-t>c :<C-u>tabclose<CR>
nnoremap <C-t>o :<C-u>tabonly<CR>
nnoremap <C-t><C-t>  :<C-u>execute 'tabnext' 1 + (tabpagenr() + v:count1 - 1) % tabpagenr('$')<CR>
nnoremap <C-t>k  gT

" tabs-and-searchesを使いやすくする
" tt:飛ぶ
" tj:進む
" tk:戻る
" tl:履歴一覧
nnoremap t  <Nop>
nnoremap tt  <C-]>
nnoremap tj  :<C-u>tag<CR>
nnoremap tk  :<C-u>pop<CR>
nnoremap tl  :<C-u>tags<CR>

""" インサートモード
" カーソル移動
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-p> <Up>
inoremap <C-n> <Down>
inoremap <C-a> <C-o>^
inoremap <C-e> <End>

" <C-y><C-y>で上の行の単語をコピー
" <C-y><C-e>で下の行の単語をコピー
inoremap <expr> <C-y><C-y> col('.')==1 ? "\<Esc>kywjpa" : "\<Esc>klywjpa"
inoremap <expr> <C-y><C-e> col('.')==1 ? "\<Esc>jywkpa" : "\<Esc>jlywkpa"

" スペース、改行をundoの区切りにする
inoremap <Space> <C-g>u<Space>
inoremap <CR> <C-g>u<CR>

""" ビジュアルモード
" v2連打で行末まで選択
vnoremap v $h

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" プラグイン設定
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ruby-debugger
" デフォルトのキーマップはコメントアウトして解除しておくこと
noremap <Leader>da :<C-u>Rdebugger<Space>
noremap <Leader>dq :<C-u>RdbStop<CR>
noremap <Leader>dw :<C-u>RdbCommand<Space>
noremap <Leader>de :<C-u>RdbEval<Space>
noremap <Leader>dx :<C-u>RdbCond<Space>
noremap <Leader>dz :<C-u>RdbCatch<Space>
noremap <Leader>dl :<C-u>RdbLog<CR>
noremap <Leader>db  :call g:RubyDebugger.toggle_breakpoint()<CR>
noremap <Leader>dv  :call g:RubyDebugger.open_variables()<CR>
noremap <Leader>dm  :call g:RubyDebugger.open_breakpoints()<CR>
noremap <Leader>dt  :call g:RubyDebugger.open_frames()<CR>
noremap <Leader>ds  :call g:RubyDebugger.step()<CR>
noremap <Leader>df  :call g:RubyDebugger.finish()<CR>
noremap <Leader>dd  :call g:RubyDebugger.next()<CR>
noremap <Leader>dc  :call g:RubyDebugger.continue()<CR>
noremap <Leader>dQ  :call g:RubyDebugger.exit()<CR>
noremap <Leader>dr  :call g:RubyDebugger.remove_breakpoints()<CR>

""" NERDTree
nmap ,to :<C-u>NERDTree<Space>
nmap ,tf :<C-u>NERDTreeFind<CR>
nmap ,tt :<C-u>NERDTreeToggle<CR>
nmap ,tB :<C-u>Bookmark<Space>
nmap ,tr :<C-u>BookmarkToRoot<Space>
nmap ,tb :<C-u>OpenBookmark<Space>

""" ref.vim
nmap ,rr :<C-u>Ref refe<Space>
nmap ,ra :<C-u>Ref alc<Space>

let g:ref_alc_start_linenumber = 39  "表示する行数
let g:ref_alc_encoding = 'UTF-8' "文字コード

""" quickrun.vim
let g:quickrun_config = {}

" 非同期で実行
" http://d.hatena.ne.jp/thinca/20100212/1265948643
let g:quickrun_config = {
\   '*': {'runmode': 'async:remote:vimproc'},
\ }

" RVMで入れたRubyを使う
" http://d.hatena.ne.jp/uasi/20110411/1302531017
if strlen($rvm_bin_path)
    let g:quickrun_config['ruby'] = {
\       'command' : 'ruby',
\       'exec'    : '$rvm_bin_path/ruby -w %s',
\       'tempfile': '{tempname()}.rb'
\   }
endif
" RVMで入れたRSpecを使う
if strlen($GEM_HOME)
    let g:quickrun_config['ruby.rspec'] = {
\       'command': "rspec",
\       'exec'   : "bundle exec rspec -l {line('.')} %s",
\       'split'  : "",
\       'tmpfile': "{tempname()}_spec.rb"
\   }
endif

""" neocomplcache
" 起動時に有効
let g:neocomplcache_enable_at_startup = 1
" snippet ファイルの保存先
let g:neocomplcache_snippets_dir='~/.vim/snippets'
" dictionary
let g:neocomplcache_dictionary_filetype_lists = {
    \ 'default' : '',
    \ 'objc' : $HOME . '/.vim/dict/objc.dict'
\ }
" 日本語をキャッシュしない
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'
" 補完候補の数
let g:neocomplcache_max_list = 10
" 1番目の候補を自動選択
let g:neocomplcache_enable_auto_select = 1
" 候補の横に表示される英数字で候補を選択できる
" let g:neocomplcache_enable_quick_match = 1
" quickmatchを毎回表示
" let g:neocomplcache_quick_match_patterns = { 'default' : '' }
" quickmatchで使用する文字
" let g:neocomplcache_quick_match_table = {
            " \'1' : 0, '2' : 1, '3' : 2, '4' : 3, '5' : 4, '6' : 5, '7' : 6, '8' : 7, '9' : 8, '0' : 9
            " \}
" _を入力したときに、それを単語の区切りとしてあいまい検索を行う
let g:neocomplcache_enable_underbar_completion = 1
" smartcaseを有効にする
let g:neocomplcache_enable_smart_case = 1
" 辞書読み込み
" noremap  <Space>d. :<C-u>NeoComplCacheCachingDictionary<Enter>
" <TAB> completion.
" http://masterka.seesaa.net/article/161781923.html
function! InsertTabWrapper()
    if pumvisible()
        return "\<c-n>"
    endif
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k\|<\|/'
        return "\<tab>"
    elseif exists('&omnifunc') && &omnifunc == ''
        return "\<c-n>"
    else
        return "\<c-x>\<c-o>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
" C-nでneocomplcache補完
" inoremap <expr><C-n>  pumvisible() ? "\<C-n>" : "\<C-x>\<C-u>\<C-p>"
" C-pでkeyword補完
" inoremap <expr><C-p> pumvisible() ? "\<C-p>" : "\<C-p>\<C-n>"
" <C-j>で候補を選択
inoremap <expr> <C-j> neocomplcache#close_popup()
" <CR>で候補を選択して改行
" inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
" <C-k>で補完をキャンセル
inoremap <expr> <C-k> neocomplcache#cancel_popup()
" <C-l>で補完候補の共通文字列を補完する
inoremap <expr> <C-l> neocomplcache#complete_common_string()
" <C-s>でスニペットの展開
imap <C-s> <Plug>(neocomplcache_snippets_expand)
smap <C-s> <Plug>(neocomplcache_snippets_expand)
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
" [edit]p でスニペットの編集
nnoremap <silent> [edit]p :<C-u>NeoComplCacheEditSnippets<CR>
" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
    let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
"autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '\%(\.\|->\)\h\w*'
let g:neocomplcache_omni_patterns.cpp = '\h\w*\%(\.\|->\)\h\w*\|\h\w*::'

""" unite.vim
" 入力モードで開始する
let g:unite_enable_start_insert=1
"" キーバインドの設定
" バッファ一覧: ,ub
" ファイル一覧: ,uf
" レジスタ一覧: ,ur
" アウトライン: ,uo
" 最近使用したファイル一覧: ,um
" 常用セット: ,uu or <C-x><C-a>
" 全部乗せ: ,ui
nnoremap [unite] <Nop>
xnoremap [unite] <Nop>
nmap ,u [unite]
xmap ,u [unite]
nnoremap <silent> [unite]b :<C-u>Unite buffer<CR>
nnoremap <silent> [unite]f :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
nnoremap <silent> [unite]r :<C-u>Unite -buffer-name=register register<CR>
nnoremap <silent> [unite]o :<C-u>Unite -buffer-name=outline outline<CR>
nnoremap <silent> [unite]m :<C-u>Unite file_mru<CR>
nnoremap <silent> [unite]u :<C-u>Unite buffer file_mru<CR>
nnoremap <silent> <C-x><C-a> :<C-u>Unite buffer file_mru<CR>
inoremap <silent> <C-x><C-a> <Esc>:Unite buffer file_mru<CR>
nnoremap <silent> [unite]i :<C-u>UniteWithBufferDir -buffer-name=files buffer file_mru bookmark file<CR>
au FileType ruby nnoremap <silent> [unite]h :<C-u>Unite ref/refe<CR>
nnoremap <silent> [unite]q :<C-u>Unite -buffer-name=quickfix -no-quit qf<CR>
nnoremap <silent> [unite]g :<C-u>Unite -buffer-name=grep qf:ex=<CR>grep<Space>

" ウィンドウを分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
" ウィンドウを縦に分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
" ESCキーを2回押すと終了する
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> q
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>q

" フィルタ文字列の置換
call unite#set_substitute_pattern('file', '\$\w\+', '\=eval(submatch(0))', 200)

call unite#set_substitute_pattern('file', '[^~.]\zs/', '*/*', 20)
call unite#set_substitute_pattern('file', '/\ze[^*]', '/*', 10)

call unite#set_substitute_pattern('file', '^@@', '\=fnamemodify(expand("#"), ":p:h")."/*"', 2)
call unite#set_substitute_pattern('file', '^@', '\=getcwd()."/*"', 1)
call unite#set_substitute_pattern('file', '^\\', '~/*')

call unite#set_substitute_pattern('file', '^;v', '~/.vim/*')
call unite#set_substitute_pattern('file', '^;r', '\=$VIMRUNTIME."/*"')

""" NERD commenter
let g:NERDCreateDefaultMappings = 0
let g:NERDSpaceDelims = 1

" <Leader>cc => コメントのトグル
" <Leader>ca => 行末にコメント追加＆編集
" (Visual)<Leader>c => 選択範囲のコメントトグル
map <Leader>cc <Plug>NERDCommenterToggle
map <Leader>ca <Plug>NERDCommenterAppend
" <Leader>cg => 行末までコメント
" <Leader>cs => 複数行コメント
" <Leader>cb => ブロック全体をコメントアウト
map <Leader>cg <Plug>NERDCommenterToEOL
map <Leader>cs <Plug>NERDCommenterSexy
map <Leader>cb <Plug>NERDCommenterMinimal

""" xmpfilter
" <S-F1>で現在行/選択行にxmpfilterを実行
map  <silent> <S-F1> !xmpfilter -a<cr>
nmap <silent> <S-F1> V<S-F1>
imap <silent> <S-F1> <ESC><S-F1>a

" <S-F2>でバッファ全体にxmpfilterを実行
nmap <silent> <S-F2> mzggVG!xmpfilter -a<cr>'z
imap <silent> <S-F2> <ESC><S-F2>

" <S-F3>で現在行/選択行に「# =>」マークを追加
vmap <silent> <S-F3> !xmpfilter -m<cr>
nmap <silent> <S-F3> V<S-F3>
imap <silent> <S-F3> <ESC><S-F3>a

" <S-F4>で現在行/選択行の「# =>」マークを削除
vmap <silent> <S-F4> ms:call RemoveRubyEval()<CR>
nmap <silent> <S-F4> V<S-F4>
imap <silent> <S-F4> <ESC><S-F4>a

function! RemoveRubyEval() range
  let begv = a:firstline
  let endv = a:lastline
  normal Hmt
  set lz
  execute ":" . begv . "," . endv . 's/\s*# \(=>\|!!\).*$//e'
  normal 'tzt`s
  set nolz
  redraw
endfunction

""" vimshell
" ,is => シェルを起動
nnoremap <silent> <Leader>is :VimShell<CR>
" ,ipy => pythonを非同期で起動
nnoremap <silent> <Leader>ipy :VimShellInteractive python<CR>
" ,irb => irbを非同期で起動
nnoremap <silent> <Leader>irb :VimShellInteractive irb<CR>
" ,ss => 非同期で開いたインタプリタに現在の行を評価させる
" (Visual),ss => 非同期で開いたインタプリタに選択行を評価させる
nnoremap <silent> <Leader>ss <S-v>:VimShellSendString<CR>
vmap <silent> <Leader>ss :VimShellSendString<CR>

""" eregex.vim
" ,/ ,? => Rubyスタイルの正規表現を使用して検索
nnoremap ,/ :M/
nnoremap ,? :M?

""" alignta
let g:unite_source_alignta_preset_arguments = [
\ ["Align at '='", '! =>\='],
\ ["Align at ':'", '@01 :' ],
\ ["Align at '|'", '|' ],
\ ["Align at ')'", '@0 )' ],
\ ["Align at ']'", '@0 ]' ],
\ ["Align at '}'", '}' ],
\]

let s:comment_leadings = '^\s*\("\|#\|/\*\|//\|<!--\)'
let g:unite_source_alignta_preset_options = [
\ ["Justify Left", '<<' ],
\ ["Justify Center", '||' ],
\ ["Justify Right", '>>' ],
\ ["Justify None", '==' ],
\ ["Shift Left", '<-' ],
\ ["Shift Right", '->' ],
\ ["Shift Left [Tab]", '<--'],
\ ["Shift Right [Tab]", '-->'],
\ ["Margin 0:0", '@0' ],
\ ["Margin 0:1", '@01'],
\ ["Margin 1:0", '@10'],
\ ["Margin 1:1", '@1' ],
\
\ 'v/' . s:comment_leadings,
\ 'g/' . s:comment_leadings,
\]
unlet s:comment_leadings

xnoremap <silent> A :Alignta! =>\=<CR>
xnoremap <silent> a: :Alignta @01 :<CR>
xmap <silent><expr> as mode() !=# 'v' ? ':Alignta! \S\+'."\<CR>" : 'as'
xnoremap al :Alignta<Space>

nnoremap <silent> [unite]a :<C-u>Unite alignta:options<CR>
xnoremap <silent> [unite]a :<C-u>Unite alignta:arguments<CR>

""" Evervim
" ,ec => ノートを作成
" ,en => ノート一覧を表示
" ,et => タグ一覧を表示
" ,es => ノートを検索
nnoremap <silent> ,ec :<C-u>EvervimCreateNote<CR>
nnoremap <silent> ,en :<C-u>EvervimNotebookList<CR>
nnoremap <silent> ,et :<C-u>EvervimListTags<CR>
nnoremap <silent> ,es :<C-u>EvervimSearchByQuery<Space>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" オートコマンド設定
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup MyAutoCmd
  autocmd!

  " 現在のウィンドウに現在行表示
  au WinLeave * set nocursorline
  au WinEnter,BufRead * set cursorline

  " ファイルの拡張子に応じたテンプレートを開く
  au BufNewFile *.rb 0r $HOME/.vim/templates/rb.tpl

  " 常に開いているファイルと同じディレクトリをカレントディレクトリにする
  " http://www15.ocn.ne.jp/~tusr/vim/vim_text2.html#mozTocId567011
  au   BufEnter *   execute ":lcd " . expand("%:p:h")

  " 日本語入力をリセット
  au BufNewFile,BufRead * set iminsert=0

  " 前回終了したカーソル行に移動
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

  " 入力モード時、ステータスラインのカラーを変更
  "augroup InsertHook
  "autocmd!
  "autocmd InsertEnter * highlight StatusLine guifg=#ccdc90
  "autocmd InsertLeave * highlight StatusLine guifg=#2E434C
augroup END


" RSpecのファイルタイプをruby.rspecにする
augroup myRSpec
  autocmd!
  autocmd BufWinEnter,BufNewFile *_spec.rb set filetype=ruby.rspec
augroup END

"autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,exept,finally,def,class

" :Utf8 でファイルをutf-8で開き直す
command! -bang -nargs=? Utf8 edit<bang> ++enc=utf-8 <args>
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

nnoremap ,jf :<C-u>JunkFile<CR>
nnoremap [edit]j :<C-u>JunkFileDay<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" カーソル下のURLをFirefoxで開く
" http://d.hatena.ne.jp/shunsuk/20110508/1304865150
function! HandleURI()
  let s:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;:]*')
  echo s:uri
  if s:uri != ""
    exec "!xdg-open \"" . s:uri . "\""
  else
    echo "No URI found in line."
  endif
endfunction

nmap ,w :call HandleURI()<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Red, Green, Refactorを可視化
let s:bgcolors = ['snow', '#CC3333', '#006600', 'navy']
let s:fgcolors = ['black', 'white', 'white', 'white' ]
let s:i = 0

function! s:ToggleBgcolor()
  let bgcolor = s:bgcolors[s:i]
  let fgcolor = s:fgcolors[s:i]
  exe 'highlight FoldColumn guibg=' . bgcolor
  exe 'highlight FoldColumn guifg=' . fgcolor
  " exe 'highlight NonText guibg=' . color
  let s:i = (s:i + 1) % len(s:fgcolors)
endfunction

command! TDDToggle call s:ToggleBgcolor()
nmap ,tdd :TDDToggle<CR>

