
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

2. **Make `"${HOME}/.config/picom/picom.conf"`**

```bash
[[ -d "${HOME}/.config/picom/picom.conf" ]] || mkdir "${HOME}/.config/picom/" ; touch "${HOME}/.config/picom/picom.conf"
```

3. **Appending this repository's `alacritty.yml` to your configuration.**

```bash
curl https://raw.githubusercontent.com/Scherso/dotfiles/main/.config/picom/picom.conf > "${HOME}/.config/picom/picom.conf"
```

[picom]:     https://github.com/yshui/picom
