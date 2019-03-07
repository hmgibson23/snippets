" Z - cd to recent / frequent directories
command! -nargs=* Z :call Z(<f-args>)
function! Z(...)
  if a:0 == 0
    let list = split(system('fasd -dlR'), '\n')
    let path = tlib#input#List('s', 'Select one', list)
  else
    let cmd = 'fasd -d -e printf'
    for arg in a:000
      let cmd = cmd . ' ' . arg
    endfor
    let path = system(cmd)
  endif
  if isdirectory(path)
    echo path
    exec 'cd ' . path
  endif
endfunction

" WIP this one - aim is to call fasd and choose a dir to run :Files on
command! -nargs=* ZL lua require("hulua.init").ZLua()

" Build the current Dockerfile
command! -nargs=1 DockerBuild lua require("hulua.init").DockerBuildL("test")
