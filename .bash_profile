# ~/.profile があれば、読み込む
if [ -f "$HOME/.profile" ]; then
    source "$HOME/.profile"
fi

echo ".bash_profile is loaded"
