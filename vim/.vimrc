" Set colorscheme
set background=dark

" Set leader key
let mapleader = "\<space>"
let maplocalleader = "\<space>"

filetype on
syntax on

set backspace=indent,eol,start
set number
set clipboard=unnamedplus
set laststatus=0
set showtabline=0
set encoding=utf-8
set mouse=a
set listchars=tab:▸\ ,trail:·,nbsp:⍽
set list
set belloff=all
set display=lastline
set history=10000
set nostartofline
set scrolloff=10
set updatetime=250
set timeoutlen=300

set splitbelow
set splitright
set autoread
set hidden

set ignorecase
set smartcase

set smartindent
set autoindent
set breakindent

set expandtab
set shiftwidth=4
set tabstop=4
set smarttab

set showmatch
set incsearch
set nohlsearch

set noswapfile
set nobackup
set nowritebackup

set undofile
set undodir=~/.vim/undodir

set completeopt=menuone,noinsert,noselect
set wildmenu
set wildoptions=tagfile
set pumheight=15
set path+=**

set termguicolors
set t_Co=256
set showcmd
set cursorline
set noconfirm

nnoremap <Space> <Nop>
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>
nnoremap <C-c> :bd<CR>
nnoremap <leader>b :ls<CR>:b<Space>
nnoremap <leader>? :help<CR>
nnoremap <silent> <Esc> :nohlsearch<CR>
nnoremap <expr> j v:count == 0 ? 'gj' : 'j'
xnoremap <expr> j v:count == 0 ? 'gj' : 'j'
nnoremap <expr> k v:count == 0 ? 'gk' : 'k'
xnoremap <expr> k v:count == 0 ? 'gk' : 'k'
tnoremap <silent> <Esc><Esc> <C-\><C-n>
vnoremap p P
nnoremap <leader>q :copen<CR>
nnoremap <leader>e :e<Space>
nnoremap <leader>r :%s///gc
xnoremap <leader>r :s///gc

" Change cursor in normal and insert
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

colorscheme sorbet
hi Normal guibg=NONE ctermbg=NONE
hi NonText guibg=NONE ctermbg=NONE
hi Comment cterm=NONE gui=NONE
hi FloatBorder ctermbg=NONE guibg=NONE
hi SignColumn guibg=NONE ctermbg=NONE
hi Float ctermbg=NONE guibg=NONE

" netwr
let g:netrw_browser_x_viewer = 'xdg-open'
let g:netrw_banner = 1
let g:netrw_liststyle = 1
let g:netrw_preview = 1
let g:netrw_keepdir = 0
let g:netrw_localcopydircmd = 'cp -r'

nnoremap - :e .<CR>

augroup NetrwSettings
  autocmd!
  autocmd FileType netrw call s:SetupNetrwMappings()
augroup END

function! s:SetupNetrwMappings()
  nnoremap <buffer> <C-c> :bd<CR>
  nnoremap <buffer> <leader>? :help netrw-quickhelp<CR>
endfunction

" quickfixlist and location list
augroup AutoQF
  autocmd!
  autocmd FileType qf call SetupQFKeymaps()
  autocmd WinLeave * call s:AutoCloseQF()
augroup END

function! SetupQFKeymaps()
  nnoremap <buffer> <leader>r :cdo s///gc
  nnoremap <buffer> q :cclose<CR>:lclose<CR>
  nnoremap <buffer> <Esc> :cclose<CR>:lclose<CR>
endfunction

function! s:AutoCloseQF()
  if &filetype == 'qf' && pumvisible() == 0
    lclose
    cclose
  endif
endfunction

" Find command adapted
function! Find(filename)
  let error_file = tempname()
  let cmd = 'find . -type f \( ! -path ''./.git/*'' -and ! -path ''./node_modules/*'' -and ! -path ''./.venv/*'' -and ! -path ''./.elixir_ls/*'' \) -iname *''' . a:filename . '''* | sed ''s/$/:1: /'' > ' . error_file
  echomsg cmd
  call system(cmd)
  exe 'cgetfile ' . error_file
  copen
endfunction

command! -nargs=1 Find call Find(<f-args>)
nnoremap <leader>f :Find<Space>

" Grep command adapted
function! Grep(pattern)
  let error_file = tempname()
  let cmd = 'grep --exclude-dir={.git,node_modules,.venv,.elixir_ls} -rinH ''' . a:pattern . ''' . > ' . error_file
  call system(cmd)
  exe 'cgetfile ' . error_file
  copen
endfunction

command! -nargs=1 Grep call Grep(<f-args>)
nnoremap <leader>/ :Grep<Space>
nnoremap <leader>. :Grep<Space><C-R><C-W><CR>

" Sessions
command! WriteSession mksession! .session.vim
command! ReadSession silent! source .session.vim
command! DeleteSession !rm .session.vim

augroup Sessions
  autocmd!
  autocmd VimEnter * if argc() == 0 && filereadable('.session.vim') | execute 'ReadSession' | endif
  autocmd VimLeave * if argc() == 0 && filereadable('.session.vim') | execute 'WriteSession' | endif
augroup END

nnoremap <leader>sw :WriteSession<CR>
nnoremap <leader>sr :ReadSession<CR>
nnoremap <leader>sd :DeleteSession<CR>
