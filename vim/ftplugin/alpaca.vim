if exists("b:did_alpaca_ftplugin")
  finish
endif
let b:did_alpaca_ftplugin = 1

function! AlpacaFormat()
  execute 'silent !~/src/lang3/format-in-place.sh %'
endfunction

nnoremap <localleader>f :call AlpacaFormat()

augroup ft_alpaca
  autocmd!
  autocmd! BufWritePost *.alpaca call AlpacaFormat()
augroup END
