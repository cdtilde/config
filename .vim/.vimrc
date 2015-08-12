set nocompatible
execute pathogen#infect()
set term=xterm-256color
" ================ General Config ====================
set history=9000                "Store lots of :cmdline history
set visualbell                  "No sounds
set hidden

inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

nnoremap j gj
nnoremap k gk
"inoremap jj <ESC>

" Custom keys
noremap <F4> :set invnumber<cr>
noremap <F5> :colorscheme wombat<cr>
noremap <F6> :colorscheme iawriter<cr>
noremap <F11> :set invfullscreen<cr>

"" Leader
let mapleader = ","

"nnoremap <Leader>b :b 
nnoremap <leader><leader> <c-^> 


" ================ Turn Off Swap Files ==============
set noswapfile
set nobackup
set nowb

" ================ Completion =======================
set wildmode=list:longest


" ================ Indentation ======================
set expandtab
set shiftwidth=2
set softtabstop=2
set autoindent
filetype plugin on
filetype indent on
syntax on

" ================ Look and Feel ====================
if has('gui_running')
  set guifont=Cousine:h14 
  set guioptions-=r
  set guioptions-=L
endif

colorscheme wombat

set fillchars=vert:\│
set number
set visualbell
set noerrorbells         

" ================ Searching ====================
set ignorecase
set nocompatible
set hlsearch
set incsearch
set smartcase
nmap \q :nohlsearch<CR>
" ================ Tags ====================
set tags=./tags,tags;

" ================ CtrlP ====================
let g:ctrlp_max_files = 1000
let g:ctrlp_working_path_mode = 0
"let g:ctrlp_user_command = 'find %s -type f  | head -10000 | grep -v ".svn"'"
"let g:ctrlp_by_filename = 1
nnoremap <Leader>p :CtrlP<cr>
nnoremap <Leader>x :so ~/.vimlocal/settings/
nnoremap <Leader>e :e ~/.vimlocal/settings/
nmap ; :CtrlPBuffer<CR>
let g:ctrlp_match_window_bottom = 0
let g:ctrlp_match_window_reversed = 0
" ================ NERDTree ====================
"nnoremap <Leader>n  :NERDTreeToggle<cr>
"nnoremap <Leader>r :NERDTree %<cr>


" ================ Project Settings ====================
"" Function to switch the root of our "project"
"" Used by the settings template to switch to a hardcoded folder

function! SwitchRoot(folder)
  let root_folder=a:folder
  echom "Changed project to " . root_folder
  execute "set tags=" . root_folder . "/.tags"
  execute "cd \"" . root_folder . "\""
  let g:ctrlp_working_path_mode = 0
  execute "nnoremap <Leader>p :CtrlP " . root_folder . "<cr>"
  let cmd = "cd " . root_folder . "; ctags -f .tags -R;"
  execute "nnoremap <Leader>t :!" . cmd
endfunction

" Enable calling the command from : prompt
"command -nargs=1 Pcd call SwitchRoot(<f-args>)

" ================ Database ====================
"nnoremap <Leader>d :ConqueTerm /Users/ezachri/.emacs.d/bteq

"function! SendSelectionToConque()
""  call conque_term#send_selected(visualmode())
"endfunction

"vnoremap <Leader>q :call SendVisualSelectionToConque()<CR>
set guioptions+=a 

function! SendVisualSelectionToConque()
  let v = s:get_visual_selection()
  call SendStringToConque(v)
endfunction

function! s:get_visual_selection()
  " Why is this not a built-in Vim script function?!
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, "\n")
endfunction

function! SendStringToConque(arg) "{{{
"echom "Here! " . a:arg
    " get most recent/relevant terminal
    let term = conque_term#get_instance()
    let current_buf = winnr() 
   " save user's sb settings
    let sb_save = &switchbuf
"    set switchbuf=usetab

    " shove visual text into @@ register
    ""let reg_save = @@
    ""sil exe "normal! `<" . a:type . "`>y"
    ""let @@ = substitute(@@, '^[\r\n]*', '', '')
    ""let @@ = substitute(@@, '[\r\n]*$', '', '')

    let text_to_send = a:arg . ";\n"  

    " go to terminal buffer
    call term.focus()
    " execute yanked text
    call term.write(text_to_send)
    stopinsert!
    " reset original values
    ""let @@ = reg_save

    " scroll buffer left
    "startinsert!
    "normal! 0zH
    "exe "sleep " . 3000
    "startinsert!
    normal! 0zH
"echom "Sent it"

   "" sil exe 'set switchbuf=' . sb_save

endfunction 

"let g:Powerline_symbols = 'fancy'
"let g:airline_powerline_fonts = 1
"set laststatus=2
"set guifont=Droid\ Sans\ Mono\ For\ Powerline:h14
"



" ================ Tagbar ====================
nnoremap <Leader>b :TagbarToggle<cr>
let g:tagbar_ctags_bin = '/usr/local/bin/ctags'
let g:tagbar_type_objc = {
	\ 'ctagstype': 'objc',
	\ 'ctagsargs': [
	\		'--options='.expand('~/.vim/tags/ctags-options-objc-source'),
	\		'--objc-kinds=-N',
	\		'--format=2',
	\		'--excmd=pattern',
	\		'--extra=',
	\		'--fields=nksaSmt',
	\		'-f -'
	\ ],
	\ 'kinds': [
	\		'i:class interface',
	\		'I:class implementation',
	\		'P:protocol',
	\		'M:method',
	\		't:typedef',
	\		'v:variable',
	\ ],
	\ 'sro': ' ',
	\}


" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor\ --all-text\ -l\ --smart-case 

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  "let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  "let g:ctrlp_use_caching = 0
endif


function SavePosition()
  let g:file_name=expand("%")
  let g:line_number=line(".")
  let g:reviewer_initials="KG" " Your initials
endfunction

function InsertComment()
  execute "normal i". g:file_name . ":" . g:line_number . ": " . g:reviewer_initials . " - "
  startinsert
endfunction
nmap ,sp :call SavePosition()<CR>
nmap ,ic :call InsertComment()<CR>

"let g:ctrlp_user_command = '~/test_find %s ""'


"noremap <silent> <Leader>m :Unite -buffer-name=recent -winheight=10 file_mru<cr>
"nnoremap <Leader>b :Unite -buffer-name=buffers -winheight=10 buffer<cr>
"nnoremap <Leader>f :Unite grep:.<cr>
"" CtrlP search
"call unite#filters#matcher_default#use(['matcher_fuzzy'])
"call unite#filters#sorter_default#use(['sorter_rank'])
"call unite#custom#source('file_rec/async','sorters','sorter_rank')
"" replacing unite with ctrl-p
"nnoremap <silent> <C-0> :Unite -start-insert -buffer-name=files -winheight=10 file_rec/async<cr>
vmap <C-c><C-c> <Plug>SendSelectionToTmux
nmap <C-c><C-c> <Plug>NormalModeSendToTmux
nmap <C-c>r <Plug>SetTmuxVars
