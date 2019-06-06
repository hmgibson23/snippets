;;; mpdel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mpdel" "mpdel.el" (0 0 0 0))
;;; Generated autoloads from mpdel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel" '("mpdel-")))

;;;***

;;;### (autoloads nil "mpdel-core" "mpdel-core.el" (0 0 0 0))
;;; Generated autoloads from mpdel-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel-core" '("mpdel-core-")))

;;;***

;;;### (autoloads nil "mpdel-nav" "mpdel-nav.el" (0 0 0 0))
;;; Generated autoloads from mpdel-nav.el

(autoload 'mpdel-nav-open-artists "mpdel-nav" "\
Display all artists in the MPD database.

\(fn)" t nil)

(autoload 'mpdel-nav-open-albums "mpdel-nav" "\
Display all albums in the MPD database.

\(fn)" t nil)

(autoload 'mpdel-nav-open-stored-playlists "mpdel-nav" "\
Display all stored playlists in the MPD database.

\(fn)" t nil)

(autoload 'mpdel-nav-search-by-artist "mpdel-nav" "\
Display all songs whose artist's name match NAME.
Interactively, ask for NAME.

\(fn NAME)" t nil)

(autoload 'mpdel-nav-search-by-album "mpdel-nav" "\
Display all songs whose album's name match NAME.
Interactively, ask for NAME.

\(fn NAME)" t nil)

(autoload 'mpdel-nav-search-by-title "mpdel-nav" "\
Display all songs matching TITLE.
Interactively, ask for TITLE.

\(fn TITLE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel-nav" '("mpdel-nav-")))

;;;***

;;;### (autoloads nil "mpdel-playlist" "mpdel-playlist.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from mpdel-playlist.el

(autoload 'mpdel-playlist-open "mpdel-playlist" "\
Open a buffer with PLAYLIST, current playlist if nil.

\(fn &optional PLAYLIST)" t nil)

(autoload 'mpdel-playlist-open-stored-playlist "mpdel-playlist" "\
Ask for a stored playlist and open it.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel-playlist" '("mpdel-playlist-")))

;;;***

;;;### (autoloads nil "mpdel-song" "mpdel-song.el" (0 0 0 0))
;;; Generated autoloads from mpdel-song.el

(autoload 'mpdel-song-open "mpdel-song" "\
Open a buffer to display information about SONG.
If SONG is nil, use current song instead.

When SONG is nil, the buffer updates itself to keep showing
latest song.  Additionally, the buffer lets the user control
playback.

\(fn &optional SONG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel-song" '("mpdel-song-")))

;;;***

;;;### (autoloads nil nil ("mpdel-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mpdel-autoloads.el ends here
