;;; libmpdel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "libmpdel" "libmpdel.el" (0 0 0 0))
;;; Generated autoloads from libmpdel.el

(autoload 'libmpdel-playback-set-volume "libmpdel" "\
Set volume to VOLUME.

\(fn VOLUME)" t nil)

(autoload 'libmpdel-playback-next "libmpdel" "\
Play next song in the playlist.

\(fn)" t nil)

(autoload 'libmpdel-playback-previous "libmpdel" "\
Play previous song in the playlist.

\(fn)" t nil)

(autoload 'libmpdel-play "libmpdel" "\
Start playing.

\(fn)" t nil)

(autoload 'libmpdel-stop "libmpdel" "\
Stop playing.  See also `libmpdel-playback-play-pause'.

\(fn)" t nil)

(autoload 'libmpdel-playback-play-pause "libmpdel" "\
Toggle between play and pause.
See also `libmpdel-playback-stop'.

\(fn)" t nil)

(autoload 'libmpdel-playback-seek "libmpdel" "\
Seeks to the position TIME within the current song.

TIME is a string indicating a number of seconds, fractions
allowed.  If prefixed by + or -, then the time is relative to
the current playing position.

If HANDLER is non-nil, execute it with no parameter when seek
succeeds.

\(fn TIME &optional HANDLER)" t nil)

(autoload 'libmpdel-database-update "libmpdel" "\
Update the music database for URI, everything if nil.
Updates the music database: find new files, remove deleted files,
update modified files.

URI is a particular directory or song/file to update.  If you do
not specify it, everything is updated.

\(fn &optional URI)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "libmpdel" '("libmpdel-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; libmpdel-autoloads.el ends here
