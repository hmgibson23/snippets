PAC := sudo pacman -Syy
XBPS := xbps-install -S
pkg := xautolock ranger arc-theme emacs-gtk3 herbsluftwm connman awesomewm xrdb unzip libXrandr-devel alsa-utils pulseaudio ConsoleKit2 arandr ntp xbacklight aspell aspell-en
terminal_pkgs ?= fzf nvim ripgrep fish fasd fd zsh emacs vim stow bspwm pass polybar rofi nitrogen compton w3m mbsync msmtp
suckless := slock st dwm
suckless_dir := ${HOME}/suckless
