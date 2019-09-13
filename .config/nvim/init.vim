
let mapleader = ","
set path+=**

if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'dense-analysis/ale'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } | Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-commentary'
Plug 'vim-airline/vim-airline'
call plug#end()

" Here begins the actual config

" Plugins
" Netrw (Builtin file manager)
let g:netrw_banner=0 " hide banner
let g:netrw_altv=1 " open new things on right
let g:netrw_liststyle=3 " tree view
" deoplete
let g:deoplete#enable_at_startup = 1
" Darcula
color dracula
" Rooter (changes working directory)
let g:rooter_change_directory_for_non_project_files = ''

" Some useful things
" Relative line numbers
set number relativenumber
set nu rnu
" Highlight cursor
set cursorline
" ctags (requires ctags)
command! MakeTags !ctags -R .
" Highlight trailing whitespace
highlight TrailingWhitespace ctermbg=red guibg=red
match TrailingWhitespace /\s\+$/
" Do the same for tabs
highlight TrailingWhitespace ctermbg=red guibg=red
exec "set listchars=tab:\uBB\uBB"
set list
match TrailingWhitespace /\t/
" Spaces instead of tabs
set tabstop=4 " width of hard tabstop
set softtabstop=0 " makes soft tabs do nothing???
set expandtab " insert spaces instead of tabs
set shiftwidth=4 " size of a ident in spaces

" Open Config
command! Cfg :e~/.config/nvim/init.vim

" Keybindings
nnoremap S :Files<CR>
nnoremap \| :Buffers<CR>
nnoremap \ :Ag<CR>
nnoremap ; :Commands<CR>

" Reload config on save
autocmd! bufwritepost $MYVIMRC source $MYVIMRC
