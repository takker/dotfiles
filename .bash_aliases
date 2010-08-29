############################################################
# エイリアスの定義
# ドットファイル管理フォルダに移動
alias dots='cd $HOME/projects/dotfiles'

# apt関連
alias search='apt-cache search'
alias install='sudo apt-get install -y'

alias em='emacsclient'

alias diff='colordiff -u'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias sl='ls -AlF'

############################################################
# 関数の定義
# cdの直後にlsを実行
function cdl() {
  builtin cd $1
  ls --color=auto
}

# mkdirの直後にそのディレクトリにcdを実行
function mkcd() {
  [ -n "$1" ] && mkdir -p "$@" && cd "$1";
}

## Git関連
# gst: git st -s に行番号を付ける
function gst() {
    git st -s | awk '{ printf "%2d %s %s\n", NR, $1, $2 }'
}

# gls: git st で表示されるファイルのディレクトリを表示
function gls() {
    if [ $# -eq 0 ]
    then
        gst
    else
        git st -s | awk "(NR == $1){ printf \$2}"
    fi
}