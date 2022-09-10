<img 
     align="right" width="200px"
     src="https://wiki.gentoo.org/images/b/b8/Larry-nefarius-v2.svg"
/>

### My [Gentoo][gentoo] Dotfiles, and a guided installation for [Arch][arch] and [Gentoo][gentoo] Linux.

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
> libx11 libxft libxinerama libxss libxrandr \
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
  
2. **Add `libinput` to your `INPUT_DEVICES` flag.**
```bash
INPUT_DEVICES="libinput"
```
  
3. **Configure your `VIDEO_CARDS` flag, mine for example is `radeonsi amdgpu`, more info can be found [here][xgwiki].**
  
4. **Perform a world update to apply these changes.**
```bash
emerge --ask --verbose --update --newuse --deep @world
```

5. **Install the necessary x11 dependencies.**
```bash
emerge -a \
> dev-vcs/git \
> x11-base/xorg-server x11-base/xorg-drivers x11-apps/xinit x11-apps/xmessage x11-apps/xrandr \
> x11-libs/libX11 x11-libs/libXft x11-libs/libXinerama x11-libs/libXrender x11-libs/libXrandr \
> dev-util/pkgconf
```

<br /> 

## Package Installation and Configuration

<img 
     align="right" width="30px" 
     src="https://xmonad.org/images/logo.svg" 
/>
### Installing [XMonad][xmonad] and [XMobar][xmobar] with [Stack][stack]

1. **Installing [Stack][stack].**
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

2. **Install GHC with Stack.**
```bash
stack setup
```
> NOTE: You can run `stack ghc` to configure GHC actions, and `stack ghci` to use an interactive environment. 

3. **Clone `xmonad`, `xmonad-contrib`, and `xmobar` into the `xmonad` directory.** 

- Make an `XMonad` directory.
```bash
[[ ! -d "$HOME/.config/xmonad" ]] && mkdir $HOME/.config/xmonad
```

- Change your directory to `~/.config/xmonad/`
```bash
cd $HOME/.config/xmonad
```

- Clone the respective repositories. 
```bash
git clone "https://github.com/xmonad/xmonad" xmonad-git
git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
git clone "https://codeberg.org/xmobar/xmobar" xmobar-git
```

4. **Initialize Stack.**
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

5. **Building and Installing XMonad binaries.**
```bash 
# While still in $HOME/.config/xmonad/
stack install
```

    
[arch]:   https://archlinux.org
[gentoo]: https://gentoo.org
[xgwiki]: https://wiki.gentoo.org/wiki/Xorg/Guide#make.conf_configuration
[xmonad]: https://xmonad.org/
[xmobar]: https://codeberg.org/xmobar/xmobar
[stack]:  https://docs.haskellstack.org/en/stable/
