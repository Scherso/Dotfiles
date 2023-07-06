#!/usr/bin/env zsh

export ZSH="${HOME}/.oh-my-zsh"
export ZSH_THEME="powerlevel10k/powerlevel10k"
export EDITOR=nvim 

export plugins=(
    zsh-autosuggestions
    zsh-syntax-highlighting
)

# Addons --->

# Oh My ZSH
source "${ZSH}/oh-my-zsh.sh"
source "${HOME}/.aliases"

[[ -d "${HOME}/.local/bin/"                             ]] && export PATH="${PATH}:${HOME}/.local/bin/" 
[[ -d "${HOME}/.cargo/bin"                              ]] && export PATH="${PATH}:${HOME}/.cargo/bin/"
[[ -d "${HOME}/.spicetify"                              ]] && export PATH="${PATH}:${HOME}/.spicetify/"
[[ -d "${HOME}/.local/share/JetBrains/Toolbox/scripts/" ]] && export PATH="${PATH}:${HOME}/.local/share/JetBrains/Toolbox/scripts/"
[[ -f "${HOME}/.ghcup/env"                              ]] && source "${HOME}/.ghcup/env"
