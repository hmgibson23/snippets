PKG := stow bspwm rxvt-unicode pass polybar rofi nitrogen compton zsh emacs vim xautolock ranger arc-gtk-theme
OTHER := https://git.suckless.org/slock
PAC := sudo pacman -Syy
XBPS := xbps-install -S

pacman_install:
	$(PAC) ${PKG}

xbps_install:
	$(XBPS) ${PKG}

other_install:
	git clone ${OTHER}

stow:
	stow config -vvv
	stow emacs -vvv
	stow local -vvv
	stow root -vvv
	stow wallpapers -vvv
