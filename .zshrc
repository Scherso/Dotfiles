# Oh My ZSH 
export ZSH="$HOME/.oh-my-zsh"

# Theme
ZSH_THEME="agnoster"

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

# Allowing me to execute binaries installed by cargo in ~/.cargo/bin
if [ -d "$HOME/.cargo/bin" ] ; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

# Automatically Starting Xorg
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec sx sh $HOME/.xinitrc
fi

# Spicetify 
export PATH=$PATH:/home/sam/.spicetify

# GHCUP-env
[ -f "/home/sam/.ghcup/env" ] && source "/home/sam/.ghcup/env"
