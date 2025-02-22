#!/bin/bash

set -xe

if brew list --version emacs-plus@31 > /dev/null 2>&1; then
	echo "Uninstalling existing emacs-plus@31"
	brew uninstall emacs-plus@31
fi

# update formula
brew update

# Define the path to the formula and patches
FORMULA_PATH="/opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/Formula/emacs-plus@31.rb"
PATCH_DIR="$HOME/.emacs.d/patches"
TARGET_PATCH_DIR="/opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/patches/emacs-31"

# Copy the original formula for backup
cp "$FORMULA_PATH" "${FORMULA_PATH}.bak"

# Define the insertion point in the formula
INSERTION_POINT="round-undecorated-frame"

inject_patches() {
  # Process new user patches
  for patch in "$PATCH_DIR"/*.patch; do
    local patch_name
    patch_name=$(basename "$patch" .patch)
    local sha
    sha=$(shasum -a 256 "$patch" | awk '{ print $1 }')

    if ! grep -q "local_patch \"$patch_name\"" "$FORMULA_PATH"; then
      sed -i '' "/$INSERTION_POINT/a\\
  local_patch \"$patch_name\", sha: \"$sha\" # user_patch
" "$FORMULA_PATH"
      echo "Patch $patch_name added to the formula."
      inserted=true
    else
      echo "Patch $patch_name already exists in the formula."
    fi

    ln -sf "$patch" "$TARGET_PATCH_DIR/${patch_name}.patch"
  done
}

inject_patches
