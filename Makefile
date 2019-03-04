include ./config.mk
define suck_clone
	cd "$(suckless_dir)" && git clone "https://git.suckless.org/$(1)" ;
endef

define suck_make
	cd "$(suckless_dir)/$(1)" && ${MAKE} clean install ;
endef


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
