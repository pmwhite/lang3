if exists("b:current_syntax")
  finish
endif

let b:current_syntax = "alpaca"

syn keyword alpacaKeyword fun let match
syn keyword alpacaBuiltin array_length array_get print read_file int_to_string int_compare string_length string_get argv
syn match alpacaData "[A-Z][a-zA-Z0-9_]*"
syn match alpacaNumber "\<\d\+\>"
syn region alpacaString start=/"/ end=/"/ contains=alpacaEscape
syn region alpacaChar start=/'/ end=/'/ contains=alpacaEscape
syn match alpacaEscape /\\./

hi def link alpacaKeyword Keyword
hi def link alpacaBuiltin Function
hi def link alpacaData Type
hi def link alpacaNumber Number
hi def link alpacaString String
hi def link alpacaChar Character
hi def link alpacaEscape SpecialChar
