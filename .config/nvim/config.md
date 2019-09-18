# Plugins

    First we can install Plug and some plugins directly from github

## Plugins Installation

    First we can bootstrap Plug then we install the plugins. I use alot of plugins cuz im used to emacs

```vim
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')
Plug 'skywind3000/asyncrun.vim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'dense-analysis/ale'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown'
Plug 'plasticboy/vim-markdown'
Plug 'sheerun/vim-polyglot'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } | Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-commentary'
Plug 'vim-airline/vim-airline'
Plug 'jceb/vim-orgmode'
Plug 'liuchengxu/vim-which-key'
Plug 'LucHermitte/lh-vim-lib'
Plug 'LucHermitte/local_vimrc'
call plug#end()
```

## Plugin Configuration 

    Now that we have our plugins lets set our options for each plugin

### netrw

    This is the builtin file manager which technically is still a plugin

```vim
let g:netrw_banner=0 " hide banner
let g:netrw_altv=1 " open new things on right
let g:netrw_liststyle=3 " tree view
```

### Color Scheme

    For now Darcula is the default color scheme
```vim
color dracula
```

### Autocompletion

    Deoplete is used as autocompletion

```vim
let g:deoplete#enable_at_startup = 1
```

    Rooter allows us to change the default directory to a project (version control or wherever nvim was openned

```vim
let g:rooter_change_directory_for_non_project_files = ''
```

    Markdown doesnt do syntax highlighting by default so we need a plugin for that

```vim
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'viml']
```

### Which-Key

    allows for a visual show of keybindings for leader key

```vim
let mapleader = "\<Space>"
let g:maplocalleader = ','
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
```

### Async

    Makes netrw and vim-fugitive faster. Also allows me to run async shell command

```vim
let g:asyncrun_open = 8
let $PYTHONUNBUFFERED=1
```

# General Options
    
    Some better defaults to make vim more usable.

```vim
set path+=**
```

    Relative Line Numbers are dope and the current line being at 0 is useless so we make that one absolute. Also the line that nvim is currently on being highlighted makes it easier to tell which one you are on.

```vim
set number relativenumber
set nu rnu
set cursorline
```

    Ctags is cool and I should probably learn how to use them

```vim
command! MakeTags !ctags -R .
```

    Highlight trailing whitespace and tabs in red. Tabs are also utf8 bullets

```vim
" Highlight trailing whitespace
highlight TrailingWhitespace ctermbg=red guibg=red
match TrailingWhitespace /\s\+$/
" Do the same for tabs
highlight TrailingWhitespace ctermbg=red guibg=red
exec "set listchars=tab:\uBB\uBB"
set list
match TrailingWhitespace /\t/
```

    Use spaces instead of tabs

```vim
" Spaces instead of tabs
set tabstop=4 " width of hard tabstop
set softtabstop=0 " makes soft tabs do nothing???
set expandtab " insert spaces instead of tabs
set shiftwidth=4 " size of a ident in spaces
```

    Give a little bit of breathing room for the cursor on the bottom. Makes a 3 line "padding" from the bottom.

```vim
set so=2
```

    Reload the config on save

```vim
autocmd! bufwritepost ~/Dotfiles/.config/nvim/config.md source $MYVIMRC
```

# Commands

    Here are my EX commands

```vim
command! Cfg :e~/Dotfiles/.config/nvim/config.md
cmap w!! w !sudo tee > /dev/null %
```

# Keybindings

    Here are my keybindings

## Global

```vim
nnoremap S :Files<CR>
nnoremap s :te<CR>
nnoremap \| :Buffers<CR>
nnoremap \ :Ag<CR>
nnoremap ; :Commands<CR>
nnoremap <leader>g :G<CR>
nnoremap <leader>c :AsyncRun ./.build-setup.sh; $VIM_BUILD<CR>
nnoremap <leader>u :AsyncRun ./.build-setup.sh; $VIM_RUN<CR>
```

## Local

