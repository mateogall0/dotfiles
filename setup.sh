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

# List your files here as "source_in_repo destination_in_home"
files=(
    ".config/nvim/init.lua .config/nvim/init.lua"
    # Add more files here if needed
)

for file_pair in "${files[@]}"; do
    src="${file_pair%% *}"
    dest="${file_pair#* }"
    copyFile "$src" "$dest"
done
