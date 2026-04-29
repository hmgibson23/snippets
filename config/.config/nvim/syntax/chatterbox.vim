" Vim syntax file
" Language: ChatterScript (Chatterbox for GameMaker)
" Maintainer: Master Bobbo
" Latest Revision: 2025-12-01
" Based on: Yarn Spinner v2 / Chatterbox syntax

if exists("b:current_syntax")
  finish
endif

" Node separators (===)
syn match chatterboxNodeSeparator /^===\s*$/
hi def link chatterboxNodeSeparator Delimiter

" Node headers (title: NodeName)
syn match chatterboxNodeTitle /^title:\s*.*/
hi def link chatterboxNodeTitle Title

" Front matter separator (---)
syn match chatterboxFrontMatterSep /^---\s*$/
hi def link chatterboxFrontMatterSep Delimiter

" Comments (// comment)
syn match chatterboxComment /\/\/.*/
hi def link chatterboxComment Comment

" Block comments (/* ... */)
syn region chatterboxBlockComment start=/\/\*/ end=/\*\//
hi def link chatterboxBlockComment Comment

" Commands (<<command>>)
syn region chatterboxCommand start=/<</ end=/>>/ contains=chatterboxKeyword,chatterboxFunction,chatterboxVariable,chatterboxString,chatterboxNumber,chatterboxBoolean,chatterboxOperator
hi def link chatterboxCommand Function

" Keywords in commands
syn keyword chatterboxKeyword if else elseif endif set to jump contained
hi def link chatterboxKeyword Keyword

" Built-in functions (visited, etc.)
syn match chatterboxBuiltinFunc /\<visited\>/ contained
hi def link chatterboxBuiltinFunc Function

" Custom functions (our bridge functions)
syn keyword chatterboxCustomFunc set_flag get_flag modify_score get_score set_score add_money set_mystery_clue contained
syn keyword chatterboxCustomFunc trigger_email trigger_im trigger_video_call trigger_urgent_email trigger_urgent_im trigger_urgent_video_call contained
syn keyword chatterboxCustomFunc trigger_narrative trigger_code_review mark_seen has_seen end_narrative autosave trigger_ending contained
hi def link chatterboxCustomFunc PreProc

" Variables ($variable)
syn match chatterboxVariable /\$\w\+/ contained
hi def link chatterboxVariable Identifier

" Strings (in commands)
syn region chatterboxString start=/"/ skip=/\\"/ end=/"/ contained
hi def link chatterboxString String

" Numbers
syn match chatterboxNumber /\<\d\+\>/ contained
syn match chatterboxNumber /\<\d\+\.\d\+\>/ contained
hi def link chatterboxNumber Number

" Booleans
syn keyword chatterboxBoolean true false True False contained
hi def link chatterboxBoolean Boolean

" Operators (is, to, ==, !=, etc.)
syn keyword chatterboxOperator is to and or not contained
syn match chatterboxOperator /[=!<>]\+/ contained
hi def link chatterboxOperator Operator

" Choices (-> choice text)
syn match chatterboxChoice /^[ \t]*->\s*.*/
hi def link chatterboxChoice Special

" Dialogue lines (regular text, not commands or choices)
syn match chatterboxDialogue /^[^-<>=\/].*$/
hi def link chatterboxDialogue Normal

" Emphasis in dialogue (*italic* or _italic_)
syn region chatterboxEmphasis start=/\*/ end=/\*/ oneline
syn region chatterboxEmphasis start=/_/ end=/_/ oneline
hi def link chatterboxEmphasis Italic

" Strong emphasis in dialogue (**bold** or __bold__)
syn region chatterboxStrong start=/\*\*/ end=/\*\*/ oneline
syn region chatterboxStrong start=/__/ end=/__/ oneline
hi def link chatterboxStrong Bold

" Character names (at start of dialogue lines, before colon)
" This matches lines like "Boss: Hello there"
syn match chatterboxSpeaker /^\s*\w\+\s*:/
hi def link chatterboxSpeaker Identifier

let b:current_syntax = "chatterbox"
