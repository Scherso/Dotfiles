<img 
     align="right" width="200px"
     src="https://wiki.gentoo.org/images/b/b8/Larry-nefarius-v2.svg"
/>

### My [Gentoo][gentoo] Dotfiles, installation for [Arch][arch] and [Gentoo][gentoo] Linux.

<a href="https://xmonad.org">
  <img 
       align="left" width="20px" 
       src="https://xmonad.org/images/logo.svg" 
  />
</a>

<a href="https://suckless.org/">
  <img
       align="left" width="20px" height="20px" 
       src="https://suckless.org/logo.svg" 
  />
</a>

---

<p align="right">
  Larry :two_hearts:
</p>

## Dependencies 
  
<img
     align="right" width="30px"
     src="http://www.archlinux.org/logos/archlinux-icon-crystal-64.svg" 
/>

- **Arch Linux**
```bash
pacman -S \ 
> git \
> xorg-server xorg-apps xorg-xinit xorg-xmesssage \
> libx11 libxft libxinerama libxss libxrandr xclip \
> pkgconf dbus
```

- **Gentoo Linux**

<img
     align="right" width="30px"
     src="https://gentoo.org/assets/img/logo/gentoo-signet.svg"
/>
1. **Add `X`, `elogind` and `dbus` to your `USE` flag.**
```bash
USE="X elongind dbus"
```
  
2. **Configure your `VIDEO_CARDS` flag, mine for example is `radeonsi amdgpu`, more info can be found [here][xgwiki].**
  
3. **Perform a world update to apply these changes.**
```bash
emerge --ask --verbose --update --newuse --deep @world
```

4. **Install the necessary x11 dependencies.**
```bash
emerge -a \
> dev-vcs/git \
> x11-base/xorg-server x11-base/xorg-drivers x11-apps/xinit x11-apps/xmessage x11-apps/xrandr \
> x11-libs/libX11 x11-libs/libXft x11-libs/libXinerama x11-libs/libXrender x11-libs/libXrandr x11-misc/xclip \
> dev-util/pkgconf
```

<br /> 

## Package Installation and Configuration

<img 
     align="right" width="30px" 
     src="https://xmonad.org/images/logo.svg" 
/>
### Installing [XMonad][xmonad] and [XMobar][xmobar] with [Stack][stack]

1. **Make `$HOME/.local/bin` binaries executables.**

- Bash
```bash
echo 'PATH="$HOME/.local/bin/:$PATH"' >> $HOME/.bashrc
```

- Zsh
```bash
echo 'PATH="$HOME/.local/bin/:$PATH"' >> $HOME/.zshrc
```

2. **Installing [Stack][stack].**
- Client URL
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

- GNU Web Get
```bash
wget -qO- https://get.haskellstack.org/ | sh
```

- Portage (Gentoo)
```bash
emerge -a dev-haskell/stack
```

- Pacman (Arch)
```bash
pacman -S stack
```

3. **Install GHC with Stack.**
```bash
stack setup
```
> NOTE: You can run `stack ghc` to configure GHC actions, and `stack ghci` to use an interactive environment. 

4. **Clone `xmonad`, `xmonad-contrib`, and `xmobar` into the `xmonad` directory.** 

- Make an `XMonad` directory.
```bash
[[ ! -d "$HOME/.config/xmonad" ]] && mkdir $HOME/.config/xmonad/
```

- Change your directory to `~/.config/xmonad/`
```bash
cd $HOME/.config/xmonad/
```

- Clone the respective repositories. 
```bash
git clone "https://github.com/xmonad/xmonad" xmonad-git
git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
git clone "https://codeberg.org/xmobar/xmobar" xmobar-git
```

5. **Initialize Stack.**
```bash
# While still in $HOME/.config/xmonad/
stack init
```

- A file named stack.yaml will appear, you can replace the contents of that file replace with my configuration with the following command. 
```bash
curl https://raw.githubusercontent.com/Scherso/dotfiles/main/.config/xmonad/stack.yaml > $HOME/.config/xmonad/stack.yaml
```

- Alternatively, you can enter it yourself by including the following `xmobar` flags.
```yaml
flags:
  xmobar:
    with_xft: true
    with_xpm: true
```

6. **Building and Installing XMonad binaries.**
```bash 
# While still in $HOME/.config/xmonad/
stack install
```

- Once `XMonad` and `XMobar` are compiled and installed in `$HOME/.local/bin/`, test this by running:
```bash
xmonad --version
```

7. **Copying the XMonad directory from these dotfiles.**

- Cloning the repository to `$HOME/.sources/`
```bash
[[ ! -d "$HOME/.sources" ]] && mkdir $HOME/.sources/ ; git clone https://github.com/Scherso/Dotfiles $HOME/.sources/
```

- Moving the xmonad config directory to `$HOME/.config/`
```bash
mv $HOME/.sources/.config/xmonad/xmonad.hs $HOME/.config/xmonad/ && mv $HOME/.sources/.config/xmonad/xmobar $HOME/.config/xmonad/
```

8. **Verifying and debugging XMonad**

- To test if XMonad will compile and run:
```bash
xmonad --recompile
```

9. **Adding XMonad to your `.xinitrc` file**

- Make a `.xinitrc` file if you haven't already
```bash
[[ ! -f "$HOME/.xinitrc" ]] && touch $HOME/.xinitrc
```

- With the editor of your choice, open the file for editing and add the following lines
```bash
dbus-launch --exit-with-session xmonad > $HOME/.config/xmonad/log.txt
```

10. **Launch into XMonad**

- **BEFORE LAUNCHING** XMonad's default terminal in this configuration is Alacritty, you can change this by editing the file's `myTerminal` string. Contrary, you can install Alacritty with the this repository's configuration [below](https://github.com/Scherso/dotfiles#installing-and-configuring-alacritty).

- While in a tty, run 
```bash
startx
```

- For a list of keybinds, click the dropdown below

<details>
  <summary> 
    XMonad Bindings</summary>
   
  <br />
  
  - **Base XMonad Keybinds**
  
  | Keybinding | Accociated Function |
  |  :---  | :--- |
  | MOD + g | Toggle borders on a focused window |
  | MOD + SHIFT + c | kill focused window |
  | MOD + SHIFT + x | Force kill focused window |
  | MOD + space | Switch to next layout |
  | MOD + n | Refresh XMonad |
  | MOD + SHIFT + q | Quits XMonad and X Server |
  | MOD + q | Re-compiles and restarts XMonad without killing X server |
  | MOD + 1-9 | Switch to workspaces 1-9 according to the key |
  
  - **Window Managemnt**
  
  | Keybinding | Accociated Function | 
  | :--- | :--- |
  | MOD + tab | Switch focus to the next window |
  | MOD + j | Switch focus to the next window to the left |
  | MOD + k | Switch focus to the next window to the right |
  | MOD + m | Switch focus to the master window | 
  | MOD + return | Swap the master window with the focused window |
  | MOD + SHIFT + j | Swap the focused window to the left |
  | MOD + SHIFT + k | Swap the focused window to the right |
  | MOD + h | shrink focused window to the left |
  | MOD + l | shrink focused window to the right |
  | MOD + t | Tile a floating window |
  | MOD + SHIFT + F | Toggle fullscreen on a window |
  
  - **Applicatoins (Screenshots are copied to the clipboard)**
  
  | Keybindings | Accociated Function |
  | :--- | :--- |
  | MOD + SHIFT + return | Open Alacritty terminal |
  | MOD + f | Open Firefox |
  | MOD + s | Selective screenshot |
  | Print Screen | Fullscreen screenshot |
  | MOD + p | Open dmenu |
  
  - **Multimedia**
  
  | Keybindings | Accociated Function |
  | :--- | :--- |
  | Play/Pause | Play/Pause media/song |
  | Previous | Previous media/song |
  | Next | Next media/song |
  | Mute | Mute audio |
  | LowerVolume | Lower the audio volume |
  | RaiseVolume | Raise the audio volume |
  
  - **Mouse Bindings**
  
  | Mousebinding | Accociated Function |
  | :--- | :--- |
  | MOD + Left Click | Float and move window by dragging |
  | MOD + Middle Click | Move window to the top of the stack |
  | MOD + Right Click | Float and resize window by dragging |
  
</details>

<br />

### Installing and configuring [dmenu][dmenu] with this repository's configurations. 
<img 
     align="right" width="30px" 
     src="https://suckless.org/logo.svg" 
/>

1. **Cloning this repository to `$HOME/.sources/`**

- **NOTE: IF you have already cloned this repository, do not follow this step.**
```bash
[[ ! -d "$HOME/.sources" ]] && mkdir $HOME/.sources/ ; git clone https://github.com/Scherso/Dotfiles $HOME/.sources/
```

2. Move the dmenu directory to `$HOME/.config/`
```bash
mv $HOME/.sources/Dotfiles/.config/dmenu/ $HOME/.config/
```

3. Change your directory to `$HOME/.config/dmenu/` and compile and install.

- Changing the directory
```bash
cd $HOME/.config/dmenu
```

- Compiling and installing, run the following commands as root. (sudo/doas)
```bash
make clean install
```

<br />

### Installing and configuring [NeoVim][neovim].
<img
     align="right" width="30px"
     src="https://user-images.githubusercontent.com/90007553/189978733-fa2ec9bb-ee8f-4611-8d7d-5ee309f703c5.png"
/>

1. **Install NeoVim**

- Gentoo
```bash
emerge -a app-editors/neovim
```

- Arch 
```bash
pacman -S app-editors/neovim
```

```bash
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

sorry this isn't done yet i'll finish it eventually 

<br />

### Installing and configuring [Alacritty][alacritty].
<img
     align="right" width="30px"
     src="https://raw.githubusercontent.com/alacritty/alacritty/master/extra/logo/compat/alacritty-term%2Bscanlines.png"
/>

1. **Install Alacritty**

- Gentoo
```bash
emerge -a x11-terms/alacritty
```

- Arch
```bash
pacman -S alacritty
```

2. **Make `$HOME/.config/alacritty/alacritty.yml`**
```bash
[[ ! -d "$HOME/.config/alacritty/alacritty.yml" ]] && mkdir $HOME/.config/alacritty/ ; touch $HOME/.config/alacritty/alacritty.yml
```

3. **Appeding this repository's `alacritty.yml` to your configurtion.**
```bash
curl https://raw.githubusercontent.com/Scherso/dotfiles/main/.config/alacritty/alacritty.yml > $HOME/.config/alacritty/alacritty.yml
```

<br />

### Installing and configuring [Picom][picom].

1. **Install Picom**

- Gentoo
```bash
emerge -a x11-misc/picom
```

- Arch 
```bash
pacman -S picom
```

2. **Make `$HOME/.config/picom/picom.conf`**
```bash
[[ ! -d "$HOME/.config/picom/picom.conf" ]] && mkdir $HOME/.config/picom/ ; touch $HOME/.config/picom/picom.conf
```

3. **Appeding this repository's `alacritty.yml` to your configurtion.**
```bash
curl https://raw.githubusercontent.com/Scherso/dotfiles/main/.config/picom/picom.conf > $HOME/.config/picom/picom.conf
```

<br /> 

[arch]:      https://archlinux.org
[gentoo]:    https://gentoo.org
[xgwiki]:    https://wiki.gentoo.org/wiki/Xorg/Guide#make.conf_configuration
[xmonad]:    https://xmonad.org/
[xmobar]:    https://codeberg.org/xmobar/xmobar
[stack]:     https://docs.haskellstack.org/en/stable/
[dmenu]:     https://tools.suckless.org/dmenu/
[alacritty]: https://alacritty.org
[rustup]:    https://rustup.rs
[picom]:     https://github.com/yshui/picom
[neovim]:    https://neovim.io
