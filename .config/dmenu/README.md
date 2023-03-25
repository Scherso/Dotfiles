### Installing and configuring [dmenu][dmenu] with this repository's configurations.

<img
    align="right" width="30px"
    src="https://suckless.org/logo.svg"
/>

1. **Cloning this repository to `"${HOME}/.sources/"`**

- **NOTE** If you have already cloned this repository before, do not follow this step.**

```bash
[[ -d "${HOME}/.sources" ]] || mkdir ${HOME}/.sources/ ; git clone https://github.com/Scherso/Dotfiles "${HOME}/.sources/"
```

2. Move the dmenu directory to `"${HOME}/.config/"`

```bash
mv "${HOME}/.sources/Dotfiles/.config/dmenu/" "${HOME}/.config/"
```

3. Change your working directory to `"${HOME}/.config/dmenu/"`, and subsequently compiling.

- Changing your working directory.

```bash
cd "${HOME}/.config/dmenu/"
```

- Compiling and installing. You must run the following commands as root. (`sudo`/`doas`)

```bash
make clean install
```

[dmenu]:     https://tools.suckless.org/dmenu/
