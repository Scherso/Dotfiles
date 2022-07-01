# Oh My ZSH 
export ZSH="$HOME/.oh-my-zsh"

# Theme
ZSH_THEME="common"

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

# Allowing me to execute commands I authored in ~/.local/bin
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Automatically Starting Xorg
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec sx sh $HOME/.xinitrc
fi

export PATH=$PATH:/home/sam/.spicetify

[ -f "/home/sam/.ghcup/env" ] && source "/home/sam/.ghcup/env" # ghcup-env