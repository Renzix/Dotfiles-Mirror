" My neovim configuration
" Originally I had it in markdowns but I realized that its probably more vimy
" to do it in raw vimscript and use folds. So here we are.

" Bootstrap Plugins {{{
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')
Plug 'tpope/vim-vividchalk'
Plug 'dense-analysis/ale'
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-fugitive'
Plug 'sheerun/vim-polyglot'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-commentary'
Plug 'jceb/vim-orgmode'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'skywind3000/asyncrun.vim'
Plug 'embear/vim-localvimrc'
Plug 'chrisbra/SudoEdit.vim'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-entire'
Plug 'liuchengxu/vim-which-key'
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }
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

" lvimrc
" Default commands which can be overriden by .lvimrc
let g:compile_command = "make"
let g:run_command = "make run"

" Ultisnippets
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-f>"
let g:UltiSnipsJumpBackwardTrigger="<c-b>"

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
command! Cfg :e~/Dotfiles/.config/nvim/init.vim
" Sudo
command! W w !sudo tee % > /dev/null
" Change current Directory
command! R Rooter
command! D lcd %:p:h
" }}}
" Normal and Visual Keybindings {{{
nnoremap Q  @q<CR>
vnoremap Q  :norm @q<CR>
nnoremap S  :Rooter<CR>:Files<CR>
nnoremap s  :lcd %:p:h<CR>:Files<CR>
nnoremap \| :Buffers<CR>
nnoremap \  :Rg<CR>
nnoremap ;  :Commands<CR>
nnoremap g=  magg=G`a
" }}}
" Leader Keybindings {{{

" Leader Key Definition
" Generally leader is for prefixes and quick commands plugins and localleader 
" is for filetype based commands
let mapleader="\<Space>"

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

" Changing aroud registers to make copy/pasting easier
nnoremap <expr> <leader>+ ':let @+=@"<CR>'
nnoremap <expr> <leader>* ':let @*=@"<CR>'

" Local leader only file type based commands
let maplocalleader=','

" }}}
" whichkey {{{
" Bind whichkey on leader and localleader keypress
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
let g:which_key_map['*'] = 'Copy Primary'
let g:which_key_map['+'] = 'Copy Clipboard'
let g:which_key_map['`'] = 'Quickfix'
" }}}
