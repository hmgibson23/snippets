# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
		./hardware-configuration.nix
	];

# Use the systemd-boot EFI boot loader.
boot.loader.systemd-boot.enable = true;
boot.loader.efi.canTouchEfiVariables = true;
boot.cleanTmpDir = true;

sound.enable = true;
hardware.pulseaudio = {
  enable = true;
  package = pkgs.pulseaudioFull;
};
hardware.bluetooth.enable = true;

boot.blacklistedKernelModules = [ "nouveau"  ];
boot.kernelParams = [
  "nomodeset" "video=vesa:off" "vga=normal" "kvm-intel"
  "kvm-amd" "psmouse.synaptics_intertouch=1"];

boot.vesa = false;

# hardware.opengl.enable = true;
# hardware.opengl.driSupport32Bit = true;
# hardware.bumblebee.enable = true;
# hardware.bumblebee.connectDisplay = true;
# services.xserver.videoDrivers = [ "nvidia" "intel" ];

# The global useDHCP flag is deprecated, therefore explicitly set to false here.
# Per-interface useDHCP will be mandatory in the future, so this generated config
# replicates the default behaviour.
networking.useDHCP = false;
networking.hostName = "dark-matter";
networking.networkmanager.enable = true;

services.lorri.enable = true;
services.udisks2.enable = true;
services.avahi.enable = true;
services.avahi.nssmdns = true;
services.journald.extraConfig = "SystemMaxUse=500M";
services.fprintd.enable = true;
services.fprintd.package = pkgs.fprintd-thinkpad;

services.logind.extraConfig = ''
  LidSwitchIgnoreInhibited=False
  HandleLidSwitch=suspend
  HoldoffTimeoutSec=10
'';

location.longitude = 0.1278;
location.latitude = 51.5074;
services.redshift = {
  enable = true;
  temperature = {
    day = 6500;
    night = 3000;

  };
};

fonts = {
  enableFontDir = true;
  fonts = with pkgs; [
    gohufont
    nerdfonts
    opensans-ttf
    siji
  ];
};

services.xserver = {
	layout = "gb";
	xkbVariant = "colemak";
	autorun = false;
	windowManager.xmonad = {
		enable = true;
		enableContribAndExtras = true;
	};
		windowManager.default = "xmonad";
		desktopManager.xterm.enable = false;

		# displayManager.ly.enable = true;
		synaptics.enable = true;
		synaptics.accelFactor = "0.01";
		synaptics.twoFingerScroll = true;
		synaptics.additionalOptions = ''
		  Option "VertScrollDelta" "-112"
	          Option "HorizScrollDelta" "-112"
		  Option "TapButton2" "3"
	          Option "TapButton3" "2"
		'';
		xkbOptions = "ctrl:nocaps";
	};

# Select internationalisation properties.
i18n = {
	consoleFont = "Lat2-Terminus16";
	defaultLocale = "en_GB.UTF-8";
	consoleUseXkbConfig = true;
};

# Set your time zone.
time.timeZone = "Europe/London";

# Some programs need SUID wrappers, can be configured further or are
# started in user sessions.
# programs.mtr.enable = true;
programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
programs.zsh.enable = true;

# List services that you want to enable:

# Enable the OpenSSH daemon.
services.openssh.enable = true;
virtualisation.docker.enable = true;
virtualisation.docker.autoPrune.enable = true;
virtualisation.libvirtd.enable = true;


# Hyper V Share
networking.interfaces.eth1.ip4 = [{address="10.0.0.101"; prefixLength=28;}];

# Open ports in the firewall.
# networking.firewall.allowedTCPPorts = [ ... ];
# networking.firewall.allowedUDPPorts = [ ... ];
# Or disable the firewall altogether.
# networking.firewall.enable = false;

# Enable CUPS to print documents.
# services.printing.enable = true;

# Enable sound.
# sound.enable = true;
# hardware.pulseaudio.enable = true;

# Enable touchpad support.

nixpkgs.config = {
	allowUnfree = true; # Allow "unfree" packages.
};

# Define a user account. Don't forget to set a password with ‘passwd’.
users.users.hugo = {
	isNormalUser = true;
	extraGroups = [ "wheel" "docker" "networkmanager" "audio"];
};


environment.systemPackages = with pkgs; [
	tree curl firefox
	wget vim neovim
	emacs git jq docker
	fd fasd fzf tmux
	gnumake aspell xclip ag pavucontrol pass
	ruby go dunst libnotify feh redshift slock docker_compose
	polybar compton
	firefox spotify
	gnupg htop rsync unzip zip
	direnv ly mpv autoconf automake
	fontconfig freetype man-pages
	playerctl neomutt elvish xorg.xmodmap dzen2
];

 environment.shellInit = ''
       export EDITOR=vim
'';

# This value determines the NixOS release with which your system is to be
# compatible, in order to avoid breaking some software such as database
# servers. You should change this only after NixOS release notes say you
# should.
system.stateVersion = "19.09"; # Did you read the comment?

}
