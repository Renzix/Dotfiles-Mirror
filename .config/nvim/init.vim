" My neovim configuration
" Originally I had it in markdowns but I realized that its probably more vimy
" to do it in raw vimscript and use folds

" Bootstrap Plugins {{{
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')
Plug 'tpope/vim-vividchalk'
Plug 'dense-analysis/ale'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown'
Plug 'plasticboy/vim-markdown'
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

" Deoplete
let g:deoplete#enable_at_startup = 1

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
set smartcase
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
command! W SudoWrite
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
let mapleader=" "
let maplocalleader=","

" .lvimrc stuff for project management
nnoremap <expr> <localleader>c ":AsyncRun " . g:compile_command . "\<CR>"
nnoremap <expr> <localleader>r ":AsyncRun " . g:run_command . "\<CR>"
nnoremap <localleader>`  :call asyncrun#quickfix_toggle(20)<CR>

" Changing aroud registers to make copy/pasting easier
nnoremap <expr> <localleader>p ':let @+=@"<CR>'
nnoremap <expr> <localleader>P ':let @*=@"<CR>'

" }}}
