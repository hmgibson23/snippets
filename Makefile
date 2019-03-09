emacs_compile:
	emacs -batch -f batch-byte-compile emacs/.emacs.d/my-modes/*.el
	emacs -batch -f batch-byte-compile emacs/.emacs.d/*.el

stow:
	stow config -vvv
	stow emacs -vvv
	stow local -vvv
	stow root -vvv
	stow wallpapers -vvv
