" My neovim configuration
" Originally I had it in markdowns but I realized that its probably more vimy
" to do it in raw vimscript and use folds. So here we are.

" Bootstrap Plugins {{{
" TODO(Renzix): Make this work for windows
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'embear/vim-localvimrc'
Plug 'honza/vim-snippets'
Plug 'jceb/vim-orgmode'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-user'
Plug 'liuchengxu/vim-which-key'
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }
Plug 'luochen1990/rainbow'
Plug 'mbbill/undotree'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'sheerun/vim-polyglot'
Plug 'SirVer/ultisnips'
Plug 'skywind3000/asyncrun.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
call plug#end()
"}}}
" Plugin config {{{

" AsyncRun
let g:asyncrun_open = 8

" Rooter
let g:rooter_change_directory_for_non_project_files = ''
let g:rooter_manual_only = 1
let g:rooter_resolve_links = 1

" netrw
" Technically not a plugin but....
let g:netrw_banner=0 " hide banner
let g:netrw_altv=1 " open new things on right
let g:netrw_liststyle=3 " tree view
let g:netrw_winsize = 15

" dracula
color dracula


" lvimrc
" Default commands which can be overriden by .lvimrc
let g:compile_command = "make"
let g:run_command = "make run"

" Rainbow Parens
let g:rainbow_active = 1

" Ultisnippets
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-f>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"

" Gitgutter
let g:gitgutter_map_keys = 0 " Disables default mappings
let g:gitgutter_grep = 'rg'


"}}}
" Actual config {{{

" Default to posix shell
set shell=sh

" Bunch of nice variables to make better defaults
set foldmethod=marker
set scrolloff=3
set number relativenumber
set undodir="~/.local/share/neovim"
set undofile
set undolevels=10000

" Search stuff
set ignorecase smartcase
set incsearch hlsearch
set inccommand=nosplit " show live changes to what the command will do

" Actually use a mouse
set mouse=a

" Gui stuff
" set termguicolors
set emoji

" Spaces instead of tabs
set expandtab " insert spaces instead of tabs
set tabstop=4 " width of hard tabstop
set softtabstop=0 " makes soft tabs do nothing???
set shiftwidth=4 " size of a ident in spaces

" Backups
" as much as humanly possible
set backup
if !isdirectory($HOME . "/.saves/vim")
    call mkdir($HOME . "/.saves/vim", "p", 0700)
endif
set backupdir=~/.saves/vim
set backupcopy=yes

" Allows recursive searching through directories with find and other stuff
set path+=**

" cursor line should have different colors
" set cursorline
" Default Colors for CursorLine
highlight  CursorLine ctermbg=Yellow ctermfg=None
" Change Color when entering Insert Mode
autocmd InsertEnter * highlight  CursorLine ctermbg=Green ctermfg=Red
" Revert Color to default when leaving Insert Mode
autocmd InsertLeave * highlight  CursorLine ctermbg=Yellow ctermfg=None

" Highlight tabs
exec "set listchars=tab:\uBB\uBB"
set list
match ErrorMsg /\t/

" Highlight trailing whitespace
highlight ColorColumn ctermbg=darkred
call matchadd('ColorColumn', '\%81v', 100) " Lines at 100 are red
call matchadd('ColorColumn', '\%100v.\+', 120) " Lines past 120 are red
" for gnu screen
set t_Co=256

" }}}
" Auto Commands {{{

" Save when you lose window focus
autocmd FocusLost * :wa

augroup vim_files "{{{
    au!

    " Run :help for word under cursor
    autocmd filetype vim noremap <buffer> ,h <Esc>:help <C-r><C-w><CR>
augroup end "}}}


" }}}
" Ex Commands {{{
" Open my config
command! Cfg :e $MYVIMRC
" Sudo
command! W w !sudo tee % > /dev/null
" Change current Directory
" }}}
" Normal and Visual Keybindings {{{
" I never use . command and ex mode so i rebind them. the dot command is
" basically a macro so I feel like dealing with a 'default' macro reg would be
" cool and useful. Might replace Q with something else
nnoremap .  @q<CR>
nnoremap Q  qq
vnoremap Q  :norm @q<CR>
" Better escape
inoremap jj <Esc>
" undo tree
nnoremap U :UndotreeToggle<CR>
" Maybe replace this with vim surround
nnoremap s :Files<CR>
nnoremap \| :Buffers<CR>
nnoremap \ :Rg<CR>
nnoremap ; :Commands<CR>
"should be this by default cuz consistancy
nnoremap Y y$
" For some reason vim doesnt bind the alt key for anything???
" Clipboard sucks
nnoremap <M-p> "+p
vnoremap <M-p> "+p
nnoremap <M-y> "+y
vnoremap <M-y> "+y
nnoremap <M-1> 1gt
nnoremap <M-2> 2gt
nnoremap <M-3> 3gt
nnoremap <M-4> 4gt
nnoremap <M-5> 5gt
nnoremap <M-6> 6gt
nnoremap <M-7> 7gt
nnoremap <M-8> 8gt
nnoremap <M-9> 9gt
nnoremap <M-0> 0gt
" }}}
" Leader Keybindings {{{

" Leader Key Definition
" Generally leader is for prefixes and quick commands plugins and localleader 
" is for filetype based commands
let mapleader="\<Space>"

" Moving default directory
nnoremap <leader>. :lcd %:p:h<CR>
nnoremap <leader>, :Rooter<CR>

" Registers
function! s:CopyRegister()
    if a:0 > 0
    endif
    let sourceReg = nr2char(getchar())

    if sourceReg !~# '\v^[a-z0-9]*'
        echo "Invalid register given: " . sourceReg
        return
    endif

    let destinationReg = nr2char(getchar())

    if destinationReg !~# '\v^[a-z0-9]*'
        echo "Invalid register given: " . destinationReg
        return
    endif

    call setreg(destinationReg, getreg(sourceReg, 1))
    echo "Replaced register '". destinationReg ."' with contents of register '". sourceReg ."'"
endfunction
nnoremap <leader>rc :call <sid>CopyRegister()<CR>
nnoremap <leader>r* :call <sid>CopyRegister()<CR>"*
nnoremap <leader>r+ :call <sid>CopyRegister()<CR>"+

" .lvimrc stuff for project management
nnoremap <expr> <leader>pc ":AsyncRun " . g:compile_command . "\<CR>"
nnoremap <expr> <leader>pR ":AsyncRun " . g:run_command . "\<CR>"
nnoremap <leader>pe :Rooter<CR>:e .lvimrc<CR>

" Git fugitive stuff
nnoremap <leader>gg :Gstatus<CR>
nnoremap <leader>gP :Gpush origin<CR>
nnoremap <expr> <leader>gp ':Gpush '
nnoremap <leader>gf :Gfetch<CR>
nnoremap <leader>gF :Gpull<CR>
nnoremap <leader>gm :Gmerge<CR>
nnoremap <leader>gb :Gbrowse<CR>

" lsp
nnoremap <leader>ld <Plug>(coc-definition)
nnoremap <leader>lt <Plug>(coc-type-definition)
nnoremap <leader>lf <Plug>(coc-references)
nnoremap <leader>l= <Plug>(coc-format-selected)
nnoremap <leader>lr <Plug>(coc-rename)

" Open
nnoremap <leader>op :Lexplore<CR>

" Single letter binds for leader
nnoremap <leader>`  :call asyncrun#quickfix_toggle(16)<CR>
nnoremap <leader>e <C-w><C-v><C-l>:e $MYVIMRC<CR>

" Local leader only file type based commands
let maplocalleader=','

" }}}
" Whichkey {{{
" Bind whichkey on leader and localleader keypress
"
autocmd! User vim-which-key call which_key#register('<Space>', 'g:which_key_map')
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey ','<CR>
let g:which_key_map =  {}
let g:which_key_map.p =  { 
            \ 'name' : 'project',
            \ 'c' : 'compile',
            \ 'R' : 'run',
            \ 'e' : 'edit lvimrc',
            \ }
let g:which_key_map.g = {
            \ 'name' : 'git',
            \ 'p' : 'Gpush'
            \ }
let g:which_key_map.l = {
            \ 'name' : 'lsp',
            \ 'd' : 'Goto Defintion',
            \ 't' : 'Goto Type Defintion',
            \ 'f' : 'Find Reference',
            \ '=' : 'Fix',
            \ 'r' : 'Rename',
            \ }
let g:which_key_map['`'] = 'Quickfix Buffer'
let g:which_key_map['.'] = 'Root Default Directory'
let g:which_key_map[','] = 'Current Default Directory'
" }}}
