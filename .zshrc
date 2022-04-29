# Oh My ZSH 
export ZSH="$HOME/.oh-my-zsh"

# Theme
ZSH_THEME="common"

# Aliases 
source $HOME/.aliases 

# Editor 
export EDITOR=nvim 

# Plugins
plugins=(git)

# Addons --->

# Oh My ZSH
source $ZSH/oh-my-zsh.sh

# Allowing me to execute commands I authored in ~/.local/bin
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Automatically Starting Xorg
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec startx
fi

# Starting Colorls 
source $(dirname $(gem which colorls))/tab_complete.sh 
path+=(
    $(ruby -e 'puts File.join(Gem.user_dir, "bin")')
)

# ZSH Syntax Highlighting and Auto Suggestions
source $HOME/.config/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
