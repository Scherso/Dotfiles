1. **Install Alacritty**

- Gentoo

```bash
emerge -a x11-terms/alacritty
```

- Arch

```bash
pacman -S alacritty
```

2. **Make `"${HOME}/.config/alacritty/alacritty.toml"`**

```bash
[[ -d "${HOME}/.config/alacritty/alacritty.toml" ]] || mkdir "${HOME}/.config/alacritty/" ; touch "${HOME}/.config/alacritty/alacritty.toml"
```

3. **Appending this repository's `alacritty.toml` to your configuration.**

```bash
curl https://raw.githubusercontent.com/Scherso/dotfiles/main/.config/alacritty/alacritty.toml > "${HOME}/.config/alacritty/alacritty.toml"
```

[alacritty]: https://alacritty.org
