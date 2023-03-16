### Installing and configuring [NeoVim][neovim].

<img
     align="right" width="30px"
     src="https://user-images.githubusercontent.com/90007553/189978733-fa2ec9bb-ee8f-4611-8d7d-5ee309f703c5.png"
/>

1. **Install NeoVim.**

- Gentoo

```bash
emerge -a app-editors/neovim
```

- Arch 

```bash
pacman -S app-editors/neovim
```

2. **Make a `"${HOME}/.config/nvim/"` directory.**

```bash
[[ -d "${HOME}/.config/nvim ]] || mkdir "${HOME}/.config/nvim/"
```

3. **Fetch Vim-Plugged and its directories.**

```bash
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

4. **Cloning this repository to `"${HOME}/.sources/"`**

- **NOTE** If you have already cloned this repository before, do not follow this step.

```bash
[[ ! -d "${HOME}/.sources" ]] && mkdir "${HOME}/.sources/" 
git clone https://github.com/Scherso/Dotfiles "${HOME}/.sources/"
```

4. **Move the contents this repository's nvim directory to `${HOME}/.config/`**

```bash
mv $HOME/.sources/Dotfiles/.config/nvim/* "${HOME}/.config/nvim/"
```

5. **Open NeoVim and `:PlugInstall`**

- Firstly, open NeoVim by running:

```bash
nvim
```

- Secondly, once you're in NeoVim, press `<escape>` on your keyboard, and type the following, and then press enter.

```vim
:PlugInstall
```

[rustup]:    https://rustup.rs
[picom]:     https://github.com/yshui/picom
[neovim]:    https://neovim.io
