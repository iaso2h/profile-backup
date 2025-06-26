set undofile
if has("win32") || has("win64")
    let initFile = expand("$HOME/AppData/Local/nvim/init_lite.vim")
else
    let initFile = expand("$HOME/.config/nvim/init_lite.vim")
endif

if !empty(glob(initFile))
    exec("source " . initFile)
else
    echohl WarningMsg
    echomsg "Cannot find init file at: " . initFile
    echohl None
endif
