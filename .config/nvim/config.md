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
        Plug 'tpope/vim-vividchalk'
        Plug 'dense-analysis/ale'
        Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
        Plug 'airblade/vim-gitgutter'
        Plug 'airblade/vim-rooter'
        Plug 'tpope/vim-fugitive'
        Plug 'tpope/vim-markdown'
        Plug 'plasticboy/vim-markdown'
        Plug 'sheerun/vim-polyglot'
        Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
        Plug 'tpope/vim-commentary'
        Plug 'vim-airline/vim-airline'
        Plug 'jceb/vim-orgmode'
        Plug 'SirVer/ultisnips'
        Plug 'honza/vim-snippets'
        Plug 'skywind3000/asyncrun.vim'
        Plug 'embear/vim-localvimrc'
    call plug#end()
```

## Plugin Configuration 

    Now that we have our plugins lets set our options for each plugin

### AsyncRun

    We should allow AsyncRun to auto open a window

```vim
    let g:asyncrun_open = 20
```

### netrw

    This is the builtin file manager which technically is still a plugin

```vim
    let g:netrw_banner=0 " hide banner
    let g:netrw_altv=1 " open new things on right
    let g:netrw_liststyle=3 " tree view
```

### Color Scheme

    The colorscheme changes per day.

```vim
    let dayofweek =  2 " system("date +%w")
    color vividchalk
```

### Autocompletion

    Deoplete is used as autocompletion

```vim
    let g:deoplete#enable_at_startup = 1
```

    Rooter allows us to change the default directory to a project only on command

```vim
    let g:rooter_change_directory_for_non_project_files = ''
    let g:rooter_manual_only = 1
    let g:rooter_resolve_links = 1
```

### Markdowns

    Markdown doesnt do syntax highlighting by default so we need a plugin for
    that Make it so folding is disabled in markdown

```vim
    let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'viml']
    let g:vim_markdown_folding_disabled = 1
```

### Snippets

    Snippets r cool

```vim
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<c-f>"
    let g:UltiSnipsJumpBackwardTrigger="<c-b>"
```

# General Options

    Vim leader and localleader

```vim
    let mapleader=" "
```

    Some better defaults to make vim more usable.

```vim
    set path+=**
```

    Relative Line Numbers are dope and the current line being at 0 is useless so
    we make that one absolute. Also the line that nvim is currently on being 
    highlighted makes it easier to tell which one you are on.

```vim
    set number relativenumber
    set nu rnu
    set termguicolors
    set mouse=a
    set emoji
```

    Different color cursor for insert mode!!!

```vim
    " Enable CursorLine
    set cursorline

    " Default Colors for CursorLine
    highlight  CursorLine ctermbg=Yellow ctermfg=None

    " Change Color when entering Insert Mode
    autocmd InsertEnter * highlight  CursorLine ctermbg=Green ctermfg=Red

    " Revert Color to default when leaving Insert Mode
    autocmd InsertLeave * highlight  CursorLine ctermbg=Yellow ctermfg=None
```

    Undo's should percist after closing vim so we can add that option

```vim
    set undofile
```

    Search thingies!!! defaults to smartcase cuz thats just better and it should
    show us what changes ex commands will make.

```vim
    set smartcase ignorecase
    set inccommand=nosplit
```

    Highlight trailing whitespace and tabs in red. Tabs are also utf8 bullets also lines over 80

```vim
    " Do the same for tabs
    exec "set listchars=tab:\uBB\uBB"
    set list
    match ErrorMsg /\t/
    " Highlight trailing whitespace
    highlight ColorColumn ctermbg=darkred
    call matchadd('ColorColumn', '\%81v', 100) " Lines at 81 are red
    call matchadd('ColorColumn', '\%100v.\+', 120) " Lines past 120 are red
```


    Use spaces instead of tabs  

```vim
    " Spaces instead of tabs
    set tabstop=4 " width of hard tabstop
    set softtabstop=0 " makes soft tabs do nothing???
    set expandtab " insert spaces instead of tabs
    set shiftwidth=4 " size of a ident in spaces
```

    Give a little bit of breathing room for the cursor on the bottom. Makes a 3 line "padding"
    from the bottom.

```vim
    set so=3
```

    Reload the config on save @TODO(Renzix): Fix this so it points to ~/Dotfiles/.config/nvim/config.md

```vim
    autocmd! bufwritepost $HOME/Dotfiles/.config/nvim/config.md source $MYVIMRC
```

# Commands

    Here are my EX commands

```vim
    " Edit config
    command! Cfg :e~/Dotfiles/.config/nvim/config.md
    " Save as sudo
    cmap w!! w !sudo tee > /dev/null %
    command! R Rooter
```

# Keybindings

    Here are my keybindings. These ones are "better defaults

```vim
    " Really useful keybinds that deserve a whole key
    nnoremap Q  @q<CR>
    vnoremap Q  :norm @q<CR>
    nnoremap S  :Rooter<CR>:Files<CR>
    nnoremap s  :Files<CR>
    nnoremap \| :Buffers<CR>
    nnoremap \  :Ag<CR>
    nnoremap ;  :Commands<CR>
    nnoremap g=  magg=G`a
```

    Leader based keybinds

```vim
    " Other Keybinds which are very useful
    nnoremap <leader>`  :call asyncrun#quickfix_toggle(20)<CR>
    let g:compile_command = "make" " gets overriden by .lvimrc
    nnoremap <expr> <leader>c ":AsyncRun " . g:compile_command . "\<CR>"
    let g:run_command = "make run" " gets overriden by .lvimrc
    nnoremap <expr> <leader>r ":AsyncRun " . g:run_command . "\<CR>"
    set pastetoggle=<leader>z
```
