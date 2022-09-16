# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Oh My ZSH 
export ZSH="$HOME/.oh-my-zsh"

# Theme
ZSH_THEME="powerlevel10k/powerlevel10k"

# Aliases 
source $HOME/.aliases 

# Editor 
export EDITOR=nvim 

# Plugins
plugins=(
    zsh-autosuggestions
    zsh-syntax-highlighting
)

# Addons --->

# Oh My ZSH
source $ZSH/oh-my-zsh.sh

# Local Binaries
[[ ! -d "$HOME/.local/bin/" ]] || PATH=$PATH:$HOME/.local/bin 

# GHCUP-env
[[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env"

# Spicetify
[[ ! -d "$HOME/.spicetify" ]] || export PATH=$PATH:$HOME/.spicetify

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Automatically Starting X Server
if [[ -z "${DISPLAY}" ]] && [[ "${XDG_VTNR}" -eq 1 ]] ; then
  exec startx
fi
