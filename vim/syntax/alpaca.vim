if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "alpaca"

syn case match

set synmaxcol=250

syn keyword alpacaKeyword let
syn keyword alpacaKeyword fun
syn keyword alpacaKeyword match
syn match alpacaComment "%.*\n"
syn match alpacaData "[A-Z][a-zA-Z0-9_]*"
syn region alpacaString start=+"+ skip=/\v(\\x?\x+|\\)@<!\\"|""/ end=+"+ keepend
syn region alpacaString start=+[uU]\=\z("""\)+ skip=+\\["']+ end="\z1" keepend

hi def link alpacaKeyword Keyword
hi def link alpacaVariable PreProc
hi def link alpacaComment Comment
hi def link alpacaString String


syntax sync minlines=200
