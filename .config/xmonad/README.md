### Installing [XMonad][xmonad] and [XMobar][xmobar] with [Stack][stack]

<img 
    align="right" width="30px" 
    src="https://xmonad.org/images/logo.svg" 
/>

1. **Make `${HOME}/.local/bin` binaries executables.**

- Bash

```bash
echo 'PATH="${HOME}/.local/bin/:${PATH}"' >> "${HOME}/.bashrc"
```

- Z-Shell

```bash
echo 'PATH="${HOME}/.local/bin/:${PATH}"' >> "${HOME}/.zshrc"
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

- Gentoo

```bash
emerge -a dev-haskell/stack
```

- Arch

```bash
pacman -S stack
```

3. **Install GHC with Stack.**

```bash
stack setup
```

> NOTE: You can run `stack ghc` to configure GHC actions, and `stack ghci` to use an interactive environment. 

4. **Make a `${HOME}/.config/xmonad/` directory.**

```bash
[[ ! -d "${HOME}/.config/xmonad" ]] && mkdir "${HOME}/.config/xmonad/" || cd "${HOME}/.config/xmonad" ; cd "${_}"
```

5. **Copying the XMonad directory from this respository.**

- Cloning the repository to `${HOME}/.sources/`

```bash
[[ -d "${HOME}/.sources" ]] || mkdir "${HOME}/.sources/" ; git clone "https://github.com/Scherso/Dotfiles" "${HOME}/.sources/"
```

- Moving the XMonad directory to `${HOME}/.config/xmonad`

```bash
mv $HOME/.sources/.config/xmonad/* "${HOME}/.config/xmonad/"
```

6. **Building and Installing XMonad binaries.**

- While still in `${HOME}/.config/xmonad/`

```bash 
stack install
```

- Once completed, run the following while still in the `${HOME}/.config/xmonad/` as your working directory.

```bash
./build
```

- Once `XMonad` and `XMobar` have been compiled and installed to `${HOME}/.local/bin/`, you can test the binaries by running:

```bash
xmonad --version
```

8. **Verifying and debugging XMonad**

- To test if XMonad will compile and run:

```bash
xmonad --recompile
```

9. **Adding XMonad to your `.xinitrc` file**

- Make a `.xinitrc` file if you haven't already

```bash
[[ -f "${HOME}/.xinitrc" ]] || touch "${HOME}/.xinitrc"
```

- Add the `dbus-launch` command to your `.xinitrc` file to start XMonad on `startx`
```bash
echo "dbus-launch --exit-with-session xmonad > ${HOME}/.config/xmonad/log.txt" > "${HOME}/.xinitrc"
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
        XMonad Bindings
    </summary>

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

[arch]:      https://archlinux.org
[gentoo]:    https://gentoo.org
[xgwiki]:    https://wiki.gentoo.org/wiki/Xorg/Guide#make.conf_configuration
[xmonad]:    https://xmonad.org/
[xmobar]:    https://codeberg.org/xmobar/xmobar
[stack]:     https://docs.haskellstack.org/en/stable/
[dmenu]:     https://tools.suckless.org/dmenu/
[alacritty]: https://alacritty.org