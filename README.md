<img 
    align="right" width="200px"
    src="https://wiki.gentoo.org/images/b/b8/Larry-nefarius-v2.svg"
/>

### My [Gentoo][gentoo] Linux Dotfiles

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

> See directories in [`.config`](https://github.com/Scherso/Dotfiles/tree/main/.config) for documentation on my configurations. 

[arch]:      https://archlinux.org
[gentoo]:    https://gentoo.org
