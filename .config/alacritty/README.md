1. **Install Alacritty**

- Gentoo

```bash
emerge -a x11-terms/alacritty
```

- Arch

```bash
pacman -S alacritty
```

2. **Make `"${HOME}/.config/alacritty/alacritty.yml"`**

```bash
[[ -d "${HOME}/.config/alacritty/alacritty.yml" ]] || mkdir "${HOME}/.config/alacritty/" ; touch "${HOME}/.config/alacritty/alacritty.yml"
```

3. **Appending this repository's `alacritty.yml` to your configuration.**

```bash
curl https://raw.githubusercontent.com/Scherso/dotfiles/main/.config/alacritty/alacritty.yml > "${HOME}/.config/alacritty/alacritty.yml"
```

[alacritty]: https://alacritty.org
