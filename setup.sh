#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

copyFile() {
    local src="$SCRIPT_DIR/$1"
    local dest="$HOME/$2"

    mkdir -p "$(dirname "$dest")"
    cp -f "$src" "$dest"
    echo "[COPY] $src -> $dest"
}

files=(
    ".vimrc .vimrc"
    ".config/i3/config .config/i3/config"
    ".config/nvim/init.lua .config/nvim/init.lua"
    ".config/kitty/kitty.conf .config/kitty/kitty.conf"
    ".bashrc .bashrc"
    ".tmux.conf .tmux.conf"
    ".emacs .emacs"
)

for file_pair in "${files[@]}"; do
    src="${file_pair%% *}"
    dest="${file_pair#* }"
    copyFile "$src" "$dest"
done
