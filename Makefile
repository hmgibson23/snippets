define suck_clone
	cd "$(suckless_dir)" && git clone "https://git.suckless.org/$(1)" ;
endef

define suck_make
	cd "$(suckless_dir)/$(1)" && ${MAKE} clean install ;
endef

PAC := sudo pacman -Syy
XBPS := xbps-install -S
pkg := stow bspwm rxvt-unicode pass polybar rofi nitrogen compton zsh emacs vim xautolock ranger arc-theme emacs-gtk3 herbsluftwm connman awesomewm xrdb unzip libXrandr-devel alsa-utils pulseaudio ConsoleKit2 arandr ntp xbacklight aspell aspell-en
suckless := slock st dwm
suckless_dir := ${HOME}/suckless

pacman_install:
	$(PAC) ${pkg}

xbps_install:
	$(XBPS) ${pkg}

suckless_clone:
	mkdir -p "$(suckless_dir)"
	$(foreach i,$(suckless), $(call suck_clone,$(i)))

suckless_make:
	$(foreach i,$(suckless), $(call suck_make,$(i)))

suckless_install: suckless_clone suckless_make

emacs_compile:
	emacs -batch -f batch-byte-compile emacs/.emacs.d/my-modes/*.el
	emacs -batch -f batch-byte-compile emacs/.emacs.d/*.el

stow:
	stow config -vvv
	stow emacs -vvv
	stow local -vvv
	stow root -vvv
	stow wallpapers -vvv
