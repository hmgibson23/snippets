fun! SetupCommandAlias(from, to)
  exec 'cnoreabbrev <expr> '.a:from
        \ .' ((getcmdtype() is# ":" && getcmdline() is# "'.a:from.'")'
        \ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfun

call SetupCommandAlias("grep","GrepperGrep")

command! -nargs=* -complete=shellcmd Rsplit execute "new | r! <args>"
command! -nargs=* -complete=shellcmd Rtab execute "tabnew | r! <args>"
