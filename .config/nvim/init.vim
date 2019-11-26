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
Plug 'chrisbra/SudoEdit.vim'
Plug 'dense-analysis/ale'
Plug 'embear/vim-localvimrc'
Plug 'honza/vim-snippets'
Plug 'jceb/vim-orgmode'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-user'
Plug 'liuchengxu/vim-which-key'
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }
Plug 'sheerun/vim-polyglot'
Plug 'SirVer/ultisnips'
Plug 'skywind3000/asyncrun.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-vividchalk'
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


" vividchalk
color vividchalk

" Ale
let g:ale_completion_enabled = 1
let g:ale_linters = {
  \   'python': ['flake8', 'mypy', 'pylint', 'pyls'],
  \   'rust': ['cargo', 'rls'],
  \   'cpp' : ['clangd']
  \}

" lvimrc
" Default commands which can be overriden by .lvimrc
let g:compile_command = "make"
let g:run_command = "make run"

" Ultisnippets
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-f>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"

" Gitgutter
let g:gitgutter_map_keys = 0 " Disables default mappings
let g:gitgutter_grep = 'rg'

"}}}
" Actual config {{{

" Bunch of nice variables to make better defaults
set foldmethod=marker
set scrolloff=3
set number relativenumber
set undofile

" Search stuff
set ignorecase smartcase
set incsearch hlsearch
set inccommand=nosplit " show live changes to what the command will do

" Actually use a mouse
set mouse=a

" Gui stuff
set termguicolors
set emoji

" Spaces instead of tabs
set expandtab " insert spaces instead of tabs
set tabstop=4 " width of hard tabstop
set softtabstop=0 " makes soft tabs do nothing???
set shiftwidth=4 " size of a ident in spaces

" Allows recursive searching through directories with find and other stuff
set path+=**

" cursor line should have different colors
set cursorline
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
call matchadd('ColorColumn', '\%81v', 100) " Lines at 81 are red
call matchadd('ColorColumn', '\%100v.\+', 120) " Lines past 120 are red

" }}}
" Ex Commands {{{
" Open my config
command! Cfg :e $MYVIMRC
" Sudo
command! W w !sudo tee % > /dev/null
" Change current Directory
" }}}
" Normal and Visual Keybindings {{{
" Old bind which nobody uses
nnoremap Q  @q<CR>
vnoremap Q  :norm @q<CR>
" Maybe replace this with vim surround
nnoremap S  :Rooter<CR>:Files<CR>
nnoremap s :e <C-R>=expand("%:p:h") . "/"<CR>
"nnoremap s  :lcd %:p:h<CR>:Files<CR>
nnoremap \| :Buffers<CR>
nnoremap \  :Rg<CR>
" Changable
nnoremap ;  :Commands<CR>
" Prob should just use =ae cuz plugin
nnoremap g=  magg=G`a
"should be this by default cuz consistancy
nnoremap Y y$
" For some reason vim doesnt bind the alt key for anything???
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

" .lvimrc stuff for project management
nnoremap <expr> <leader>pc ":AsyncRun " . g:compile_command . "\<CR>"
nnoremap <expr> <leader>pr ":AsyncRun " . g:run_command . "\<CR>"
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
nnoremap <leader>ld :ALEGoToDefinition<CR>
nnoremap <leader>lt :ALEGoToTypeDefinition<CR>
nnoremap <leader>lf :ALEFindReferences<CR>
nnoremap <leader>l= :ALEFix<CR>
nnoremap <leader>lr :ALERename<CR>

" Single letter binds for leader
nnoremap <leader>`  :call asyncrun#quickfix_toggle(16)<CR>

" Local leader only file type based commands
let maplocalleader=','

" }}}
" whichkey {{{
" Bind whichkey on leader and localleader keypress
"
autocmd! User vim-which-key call which_key#register('<Space>', 'g:which_key_map')
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey ','<CR>
let g:which_key_map =  {}
let g:which_key_map.p =  { 
            \ 'name' : 'project',
            \ 'c' : 'compile',
            \ 'r' : 'run',
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
