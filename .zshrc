# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]] ; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="${HOME}/.oh-my-zsh"
export ZSH_THEME="powerlevel10k/powerlevel10k"
export EDITOR=nvim

export plugins=(
    zsh-autosuggestions
    zsh-syntax-highlighting
)

source "${ZSH}/oh-my-zsh.sh"
source "${HOME}/.aliases"

# Path variables 
# Ensures binaries can be executed from '$HOME/.local/bin' and '$HOME/.spicetify'.
[[ -d "${HOME}/.local/bin/" ]] && export PATH="${PATH}:${HOME}/.local/bin/"
[[ -d "${HOME}/.spicetify"  ]] && export PATH="${PATH}:${HOME}/.spicetify/"
# Source configurations
# Renewing GHCUP environments and fetching P10K configurations.
[[ -f "${HOME}/.ghcup/env"  ]] && source "${HOME}/.ghcup/env"
[[ -f "${HOME}/.p10k.zsh"   ]] && source "${HOME}/.p10k.zsh"
